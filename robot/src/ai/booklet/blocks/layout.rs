//! The blocks of the table and their packing: how tall each block is in each
//! column.
//!
//! The row model of `Blocks` says which call every kind of hand makes at every
//! count of points.  Its rows are only a way to find that out, and the
//! vertical axis of the table means nothing, so the blocks are made from it
//! without regard to the rows, and then packed as compactly as they go, with
//! no randomness:
//!
//! * A block is a family of calls over a run of counts of points.  Cells of
//!   one family in one column, whatever rows they are in, are one band of the
//!   block, and cells of one family in neighbouring columns are one block.  So
//!   a family is one block over every unbroken run of counts of points that it
//!   covers, and it is two blocks only where the counts have a gap, where no
//!   hand of any kind makes any call of the family.  Two calls of one family
//!   are never in one column twice.
//! * A block is as thick in a column as it needs to be to show it, and one
//!   unit is enough: wherever a block exists it has at least one unit, and it
//!   has more only where there is room to spare, since every column is as tall
//!   as the table.
//! * Every column is filled from top to bottom, so no space is left inside the
//!   table.  A column with few blocks is not shorter: its blocks are thicker.
//! * The table is as tall as the column with the most blocks in it, in units.
//!   No layout can be shorter, since each block of that column needs a unit of
//!   its own, and this one is never taller, so it is the shortest there is.
//!
//! The columns are laid out from left to right.  Blocks that go on keep their
//! order and their thickness, so that a boundary between two of them runs
//! straight.  When a block ends, the thinner of its neighbours that goes on
//! takes over its units, and when one begins, it goes between the blocks whose
//! rows are nearest to its own in the row model and takes one unit from the
//! nearest block that has two or more.  So a boundary only moves where a block
//! begins or ends, and it moves in a step, by as few units as it takes.  A
//! column where nothing goes on is laid out afresh, with the spare units
//! shared out in proportion to the cells of the row model.  Every choice
//! breaks its ties by the position in the column or by the block's number,
//! which is the order of its family and then of its first column.

use super::Family;

/// One block of the table, laid out.
pub(super) struct Block {
    /// The family of its calls.
    pub(super) family: Family,
    /// The column it starts in.
    pub(super) first: usize,
    /// Its top and bottom, in units, in each column from `first` on.
    pub(super) bands: Vec<(usize, usize)>,
}

impl Block {
    /// The last column it is in.
    pub(super) fn last(&self) -> usize {
        self.first + self.bands.len() - 1
    }
    /// The widest rectangle inside the block, where a label goes, then the
    /// tallest, then the leftmost: its first column, its width in columns,
    /// and its top and bottom.  The block may be thinner at either side of it.
    pub(super) fn label_room(&self) -> (usize, usize, usize, usize) {
        self.room(|length, thickness| (length, thickness))
    }
    /// The tallest rectangle inside the block, then the widest, then the
    /// leftmost, in the same form: where a label goes that is turned to run
    /// up the block, when no label fits along it.
    pub(super) fn tall_room(&self) -> (usize, usize, usize, usize) {
        self.room(|length, thickness| (thickness, length))
    }
    /// The rectangle inside the block that is best by `key`, which is given
    /// its width in columns and its thickness in units.
    fn room(&self, key: impl Fn(usize, usize) -> (usize, usize)) -> (usize, usize, usize, usize) {
        let mut best = (0, 0, 0, 0);
        for i in 0..self.bands.len() {
            let (mut top, mut bottom) = self.bands[i];
            for (j, &(t, b)) in self.bands.iter().enumerate().skip(i) {
                top = top.max(t);
                bottom = bottom.min(b);
                if top >= bottom {
                    break;
                }
                let (length, thickness) = (j - i + 1, bottom - top);
                if key(length, thickness) > key(best.1, best.3 - best.2) {
                    best = (self.first + i, length, top, bottom);
                }
            }
        }
        best
    }
}

/// The blocks of a table and how tall the table is.
pub(super) struct Layout {
    /// The height of the table, in units.
    pub(super) height: usize,
    /// The number of columns.
    pub(super) width: usize,
    /// The blocks, by family and then by first column.
    pub(super) blocks: Vec<Block>,
}

/// Where a block is in the row model.
struct Span {
    first: usize,
    last: usize,
    /// In each column from `first` on, how many cells it has and the mean of
    /// their rows.
    cells: Vec<(usize, f64)>,
}

impl Span {
    fn has(&self, column: usize) -> bool {
        self.first <= column && column <= self.last
    }
    fn count(&self, column: usize) -> usize {
        self.cells[column - self.first].0
    }
    fn row(&self, column: usize) -> f64 {
        self.cells[column - self.first].1
    }
}

/// `extra` units shared among `weights` in proportion to them, by largest
/// remainder, the earlier one first when remainders are equal.
fn share_out(weights: &[usize], extra: usize) -> Vec<usize> {
    let total: usize = weights.iter().sum();
    let mut shares: Vec<usize> = weights.iter().map(|&w| extra * w / total).collect();
    let mut by_remainder: Vec<usize> = (0..weights.len()).collect();
    by_remainder.sort_by_key(|&i| (std::cmp::Reverse(extra * weights[i] % total), i));
    let left = extra - shares.iter().sum::<usize>();
    for &i in by_remainder.iter().take(left) {
        shares[i] += 1;
    }
    shares
}

impl Layout {
    /// The blocks of `rows`, packed.  Every row has the same length.
    pub(super) fn of(rows: &[Vec<Family>]) -> Layout {
        let width = rows[0].len();
        let mut families: Vec<Family> = rows.iter().flatten().copied().collect();
        families.sort();
        families.dedup();
        let mut spans: Vec<Span> = Vec::new();
        let mut blocks: Vec<Block> = Vec::new();
        for family in families {
            // The cells of the family in a column: how many, and their mean row.
            let cells = |c: usize| -> Option<(usize, f64)> {
                let here: Vec<usize> = (0..rows.len()).filter(|&r| rows[r][c] == family).collect();
                (!here.is_empty()).then(|| {
                    (
                        here.len(),
                        here.iter().sum::<usize>() as f64 / here.len() as f64,
                    )
                })
            };
            let mut c = 0;
            while c < width {
                let Some(first_cells) = cells(c) else {
                    c += 1;
                    continue;
                };
                let first = c;
                let mut run = vec![first_cells];
                c += 1;
                while let Some(more) = (c < width).then(|| cells(c)).flatten() {
                    run.push(more);
                    c += 1;
                }
                spans.push(Span {
                    first,
                    last: c - 1,
                    cells: run,
                });
                blocks.push(Block {
                    family,
                    first,
                    bands: Vec::new(),
                });
            }
        }
        let height = (0..width)
            .map(|c| spans.iter().filter(|s| s.has(c)).count())
            .max()
            .unwrap_or(0);

        // The blocks of the last column, top to bottom, and how thick each is.
        let mut order: Vec<usize> = Vec::new();
        let mut thick = vec![0; spans.len()];
        for c in 0..width {
            let ends = |b: &usize| spans[*b].last < c;
            let born: Vec<usize> = (0..spans.len()).filter(|&b| spans[b].first == c).collect();
            if order.iter().all(ends) {
                // Nothing goes on, so the column is laid out afresh.
                order = born;
                order.sort_by(|&a, &b| {
                    spans[a]
                        .row(c)
                        .partial_cmp(&spans[b].row(c))
                        .unwrap()
                        .then(a.cmp(&b))
                });
                let weights: Vec<usize> = order.iter().map(|&b| spans[b].count(c)).collect();
                let extra = share_out(&weights, height - order.len());
                for (&b, extra) in order.iter().zip(extra) {
                    thick[b] = 1 + extra;
                }
            } else {
                // A block that ends gives its units to a neighbour that goes on.
                for i in 0..order.len() {
                    let b = order[i];
                    if !ends(&b) {
                        continue;
                    }
                    let above = (0..i).rev().find(|&j| !ends(&order[j]));
                    let below = (i + 1..order.len()).find(|&j| !ends(&order[j]));
                    let take = match (above, below) {
                        (Some(a), Some(z)) if thick[order[z]] < thick[order[a]] => z,
                        (Some(a), _) => a,
                        (None, Some(z)) => z,
                        (None, None) => unreachable!("something goes on"),
                    };
                    thick[order[take]] += thick[b];
                    thick[b] = 0;
                }
                order.retain(|b| !ends(b));
                // A block that begins goes where its rows are, and takes a
                // unit from the nearest block that has one to spare.
                for b in born {
                    let row = spans[b].row(c);
                    let place = (0..=order.len())
                        .min_by_key(|&i| {
                            let wrong = order[..i]
                                .iter()
                                .filter(|&&o| spans[o].row(c) > row)
                                .count()
                                + order[i..]
                                    .iter()
                                    .filter(|&&o| spans[o].row(c) < row)
                                    .count();
                            (wrong, i)
                        })
                        .unwrap();
                    order.insert(place, b);
                    let donor = (0..order.len())
                        .filter(|&j| j != place && thick[order[j]] >= 2)
                        .min_by_key(|&j| (j.abs_diff(place), std::cmp::Reverse(thick[order[j]]), j))
                        .expect("a unit to spare, as the table is as tall as its fullest column");
                    thick[order[donor]] -= 1;
                    thick[b] = 1;
                }
            }
            let mut top = 0;
            for &b in order.iter() {
                blocks[b].bands.push((top, top + thick[b]));
                top += thick[b];
            }
            debug_assert_eq!(top, height);
        }
        Layout {
            height,
            width,
            blocks,
        }
    }

    /// Which block is in each unit of each column, top to bottom.
    pub(super) fn grid(&self) -> Vec<Vec<usize>> {
        let mut grid = vec![vec![usize::MAX; self.width]; self.height];
        for (id, block) in self.blocks.iter().enumerate() {
            for (i, &(top, bottom)) in block.bands.iter().enumerate() {
                for row in grid[top..bottom].iter_mut() {
                    row[block.first + i] = id;
                }
            }
        }
        grid
    }
}

#[cfg(test)]
mod tests {
    use super::super::Blocks;
    use super::*;
    use crate::ai::booklet::{small_booklet, Booklet};

    /// Checks `layout` against the row model `rows` that it is made from, and
    /// gives the height that the table had.
    fn check(rows: &[Vec<Family>], layout: &Layout) -> usize {
        let width = rows[0].len();
        assert_eq!(layout.width, width);
        // Each family is one block over every unbroken run of the counts of
        // points where the rows have it, so over the union of its cells'
        // columns, with a gap between two blocks of one family.
        let has = |family: Family, c: usize| rows.iter().any(|row| row[c] == family);
        let mut families: Vec<Family> = rows.iter().flatten().copied().collect();
        families.sort();
        families.dedup();
        for family in families {
            let mut expected = Vec::new();
            let mut c = 0;
            while c < width {
                if has(family, c) {
                    let first = c;
                    while c < width && has(family, c) {
                        c += 1;
                    }
                    expected.push((first, c - 1));
                } else {
                    c += 1;
                }
            }
            let packed: Vec<(usize, usize)> = layout
                .blocks
                .iter()
                .filter(|b| b.family == family)
                .map(|b| (b.first, b.last()))
                .collect();
            assert_eq!(packed, expected);
        }
        // Every unit of every column belongs to exactly one block.
        let mut filled = vec![vec![0; width]; layout.height];
        for block in layout.blocks.iter() {
            for (i, &(top, bottom)) in block.bands.iter().enumerate() {
                assert!(top < bottom && bottom <= layout.height);
                for row in filled[top..bottom].iter_mut() {
                    row[block.first + i] += 1;
                }
            }
        }
        assert!(filled.iter().all(|row| row.iter().all(|&n| n == 1)));
        // And no table is shorter: its fullest column has a unit for each
        // distinct block.
        let fullest = (0..width)
            .map(|c| {
                layout
                    .blocks
                    .iter()
                    .filter(|b| b.first <= c && c <= b.last())
                    .count()
            })
            .max()
            .unwrap();
        assert_eq!(layout.height, fullest);
        // One family is never twice in a column.
        for c in 0..width {
            let mut here: Vec<Family> = layout
                .blocks
                .iter()
                .filter(|b| b.first <= c && c <= b.last())
                .map(|b| b.family)
                .collect();
            let n = here.len();
            here.sort();
            here.dedup();
            assert_eq!(here.len(), n);
        }
        assert!(layout.height <= rows.len());
        rows.len()
    }

    #[test]
    fn packing_makes_a_block_of_each_run_of_a_family() {
        let booklet = small_booklet();
        let (mut before, mut after) = (0, 0);
        for page in booklet.pages.iter() {
            let Some(table) = Blocks::of(page) else {
                continue;
            };
            let layout = table.layout();
            before += check(&table.rows, &layout);
            after += layout.height;
        }
        assert!(after < before, "{} units of {}", after, before);
    }

    #[test]
    fn packing_leaves_no_gaps_in_a_hand_made_table() {
        // D is in two rows that only join in the first column, and A is in
        // two rows that never touch, but each is one block, since the rows
        // mean nothing.
        let (a, b, c, d) = (Family(1), Family(2), Family(3), Family(4));
        let rows = vec![
            vec![a, a, a, a, a, a],
            vec![d, d, b, b, c, c],
            vec![d, a, a, a, a, a],
            vec![d, d, d, d, d, d],
        ];
        let layout = Layout::of(&rows);
        check(&rows, &layout);
        assert_eq!(layout.blocks.len(), 4);
        assert_eq!(layout.height, 3);
        // Blocks that fit in fewer rows than the row model has make a shorter
        // table.
        let rows = vec![
            vec![a, a, a, a],
            vec![a, a, b, b],
            vec![c, c, b, b],
            vec![c, c, c, c],
        ];
        let layout = Layout::of(&rows);
        check(&rows, &layout);
        assert_eq!(layout.height, 3);
        // A family with a gap in its counts of points is two blocks.
        let row = vec![vec![a, b, a]];
        let layout = Layout::of(&row);
        check(&row, &layout);
        assert_eq!((layout.blocks.len(), layout.height), (3, 1));
        // A column of cells of two families is two blocks, not three.
        let column = vec![vec![a], vec![b], vec![a]];
        let layout = Layout::of(&column);
        check(&column, &layout);
        assert_eq!((layout.blocks.len(), layout.height), (2, 2));
        // A family that fills two rows is two units thick where it has room.
        let rows = vec![vec![a, a], vec![b, a], vec![a, a]];
        let layout = Layout::of(&rows);
        check(&rows, &layout);
        assert_eq!(layout.blocks[0].bands, vec![(0, 1), (0, 2)]);
    }

    /// Every system's tables, from many deals, which is too slow without
    /// optimisation: `cargo test -p robot --release packing`.
    #[test]
    #[cfg_attr(debug_assertions, ignore)]
    fn packing_checks_out_for_every_system() {
        for system in crate::ai::BiddingSystem::ALL {
            let booklet = Booklet::new(&system.convention(), 20_000, 20);
            for page in booklet.pages.iter() {
                if let Some(table) = Blocks::of(page) {
                    check(&table.rows, &table.layout());
                }
            }
        }
    }

    #[test]
    fn packing_is_the_same_every_time() {
        // Neither the layout nor the pictures of it may depend on the order
        // of a hash map, which differs between one booklet and the next.
        let render = || {
            let booklet = Booklet::new(&crate::ai::Convention::sayc(), 400, 2);
            booklet
                .pages
                .iter()
                .map(|page| booklet.block_table(page))
                .collect::<Vec<_>>()
                .join("\n")
        };
        let first = render();
        assert!(first.contains("<svg"));
        for _ in 0..3 {
            assert_eq!(first, render());
        }
    }
}
