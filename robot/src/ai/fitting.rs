//! The robot's n-tuple networks, and the fit that they share, which only
//! the tests that fit them use.  A network scores each candidate of a choice, such as each card that
//! could be played, from the table entries that the candidate looks up: the
//! sum of a weight for each entry, plus, with hidden units, a layer of them
//! that each entry also feeds (see [`Network`]).  Fitting makes the
//! candidates of most value the likeliest, where a candidate is chosen at
//! random with odds that grow exponentially with its score, by gradient
//! ascent on the mean value of that choice.

#[cfg(test)]
use rand::rngs::StdRng;
#[cfg(test)]
use rand::{Rng, SeedableRng};

/// For each choice, each candidate's entries and its value to the one who
/// chooses, the more the better.
#[cfg(test)]
pub(crate) type Choices = Vec<Vec<(Vec<usize>, f64)>>;

/// An n-tuple network with up to two layers of hidden units.  The score of
/// a candidate is the sum of the linear weights of its entries, plus the
/// output of the units: each unit of the first layer sums its weights of the
/// candidate's entries and a bias, as an NNUE accumulator is summed, a few
/// additions an entry, and takes the larger of that and 0; each unit of the
/// second layer, if there is one, does the same with its weights of the
/// first layer's units; and the last layer's units, times their output
/// weights, are added to the score.  The units let the network weigh
/// combinations of entries from different tables, and the second layer
/// combinations of those.  With no units it is the sum of weights alone.
#[derive(Clone)]
pub(crate) struct Network {
    entries: usize,
    hidden: usize,
    second: usize,
    /// The linear weights, one an entry; then the first layer's weights,
    /// `hidden` an entry, and its biases; then, with a second layer, its
    /// weights, `second` for each unit of the first, and its biases; then the
    /// output weights of the last layer.
    pub(crate) params: Vec<f64>,
}

/// What a network works out on the way to a candidate's score.
struct Forward {
    /// The first layer's sums, and the larger of each and 0.
    #[cfg_attr(not(test), expect(dead_code, reason = "only fitting reads the sums"))]
    sums: Vec<f64>,
    active: Vec<f64>,
    /// The same of the second layer, if there is one.
    #[cfg_attr(not(test), expect(dead_code, reason = "only fitting reads the sums"))]
    second_sums: Vec<f64>,
    second_active: Vec<f64>,
}

impl Network {
    /// How many parameters a network of these sizes has.
    pub(crate) fn size(entries: usize, hidden: usize, second: usize) -> usize {
        size(entries, hidden, second)
    }

    /// A network of `entries` entries, `hidden` units in its first layer
    /// and `second` in its second (none for one layer), that scores every
    /// candidate the same, so that it starts by choosing at random.  The
    /// units' weights are small and random, the same for the same `seed`, so
    /// that the units can learn different things.
    #[cfg(test)]
    pub(crate) fn new(entries: usize, hidden: usize, second: usize, seed: u64) -> Network {
        let mut rng = StdRng::seed_from_u64(seed);
        let mut network = Network { entries, hidden, second, params: vec![0.0; size(entries, hidden, second)] };
        let first = network.first_weights();
        for p in &mut network.params[first..first + entries * hidden] {
            *p = rng.gen_range(-0.1..0.1);
        }
        let second_weights = network.second_weights();
        for p in &mut network.params[second_weights..second_weights + hidden * second] {
            *p = rng.gen_range(-0.5..0.5);
        }
        network
    }

    /// The network of these sizes with `params`, such as one written out
    /// before.
    pub(crate) fn with_params(entries: usize, hidden: usize, second: usize, params: Vec<f64>) -> Network {
        assert_eq!(params.len(), size(entries, hidden, second), "the size of the network");
        Network { entries, hidden, second, params }
    }

    /// `values`, laid out as this network's weights are (its weights, or
    /// Adam's moments of them), with the entries renumbered: entry `e`
    /// becomes `moved[e]` of `entries`, keeping its values, or is dropped if
    /// that is `None`.  An entry that none moves to has values of 0.  The
    /// units' values are as they were.
    #[cfg(test)]
    pub(crate) fn with_entries_moved(&self, values: &[f64], moved: &[Option<usize>], entries: usize) -> Vec<f64> {
        assert_eq!(moved.len(), self.entries);
        assert_eq!(values.len(), self.params.len());
        let h = self.hidden;
        let new = Network { entries, hidden: h, second: self.second, params: Vec::new() };
        let mut result = vec![0.0; size(entries, h, self.second)];
        for (old, to) in moved.iter().enumerate() {
            if let Some(to) = *to {
                result[to] = values[old];
                let (from, to) = (self.first_weights() + old * h, new.first_weights() + to * h);
                result[to..to + h].copy_from_slice(&values[from..from + h]);
            }
        }
        let start = new.first_biases();
        result[start..].copy_from_slice(&values[self.first_biases()..]);
        result
    }

    /// This network grown into one of `entries` entries, `hidden` units in
    /// its first layer and `second` in its second, at least as many as it
    /// has (and a second layer only if it has one), which scores every
    /// candidate as it did: entry `e` becomes `moved[e]`, with its weights,
    /// and the entries and units that are new add nothing.  An entry that is
    /// new has weights only into the new units, and a unit that is new has
    /// weights of 0 out of it, so that neither changes a score; the new
    /// weights into the new units are the small random ones of
    /// [`Network::new`] with `seed`, so that they can learn.
    #[cfg(test)]
    pub(crate) fn grown(&self, moved: &[usize], entries: usize, hidden: usize, second: usize, seed: u64) -> Network {
        assert_eq!(moved.len(), self.entries);
        assert!(hidden >= self.hidden && second >= self.second && (second == 0) == (self.second == 0));
        let mut grown = Network::new(entries, hidden, second, seed);
        let (old_h, old_s) = (self.hidden, self.second);
        // Every new entry's weights into the old units are 0.
        for entry in 0..entries {
            let row = grown.first_weights() + entry * hidden;
            grown.params[row..row + old_h].fill(0.0);
        }
        for (old, &new) in moved.iter().enumerate() {
            grown.params[new] = self.params[old];
            let (from, to) = (self.first_weights() + old * old_h, grown.first_weights() + new * hidden);
            grown.params[to..to + old_h].copy_from_slice(&self.params[from..from + old_h]);
        }
        let (from, to) = (self.first_biases(), grown.first_biases());
        grown.params[to..to + old_h].copy_from_slice(&self.params[from..from + old_h]);
        for unit in 0..hidden {
            let to = grown.second_weights() + unit * second;
            if unit < old_h {
                let from = self.second_weights() + unit * old_s;
                grown.params[to..to + old_s].copy_from_slice(&self.params[from..from + old_s]);
            } else {
                // A new unit of the first layer feeds only the new units of
                // the second.
                grown.params[to..to + old_s].fill(0.0);
            }
        }
        let (from, to) = (self.second_biases(), grown.second_biases());
        grown.params[to..to + old_s].copy_from_slice(&self.params[from..from + old_s]);
        // The output weights of the last layer: the old units', and 0 for
        // the new ones.
        let last = if second > 0 { old_s } else { old_h };
        let (from, to) = (self.outputs(), grown.outputs());
        grown.params[to..].fill(0.0);
        grown.params[to..to + last].copy_from_slice(&self.params[from..from + last]);
        grown
    }

    fn first_weights(&self) -> usize {
        self.entries
    }
    fn first_biases(&self) -> usize {
        self.entries * (1 + self.hidden)
    }
    fn second_weights(&self) -> usize {
        self.first_biases() + self.hidden
    }
    fn second_biases(&self) -> usize {
        self.second_weights() + self.hidden * self.second
    }
    fn outputs(&self) -> usize {
        self.second_biases() + self.second
    }

    /// The layers' sums and outputs for a candidate with `entries`.
    fn forward(&self, entries: &[usize]) -> Forward {
        let h = self.hidden;
        let mut sums = self.params[self.first_biases()..self.first_biases() + h].to_vec();
        for &e in entries {
            let row = self.first_weights() + e * h;
            for (sum, w) in sums.iter_mut().zip(&self.params[row..row + h]) {
                *sum += w;
            }
        }
        let active: Vec<f64> = sums.iter().map(|s| s.max(0.0)).collect();
        let mut second_sums = self.params[self.second_biases()..self.second_biases() + self.second].to_vec();
        for (unit, &a) in active.iter().enumerate() {
            let row = self.second_weights() + unit * self.second;
            for (sum, w) in second_sums.iter_mut().zip(&self.params[row..row + self.second]) {
                *sum += w * a;
            }
        }
        let second_active = second_sums.iter().map(|s| s.max(0.0)).collect();
        Forward { sums, active, second_sums, second_active }
    }

    /// The score of a candidate with `entries`.
    pub(crate) fn score(&self, entries: &[usize]) -> f64 {
        let linear: f64 = entries.iter().map(|&e| self.params[e]).sum();
        let forward = self.forward(entries);
        let last = if self.second > 0 { &forward.second_active } else { &forward.active };
        let outputs = &self.params[self.outputs()..];
        linear + last.iter().zip(outputs).map(|(a, o)| a * o).sum::<f64>()
    }

    /// Adds `coefficient` times the gradient of the score of a candidate with
    /// `entries` to `gradient`.
    #[cfg(test)]
    pub(crate) fn add_gradient(&self, entries: &[usize], coefficient: f64, gradient: &mut [f64]) {
        for &e in entries {
            gradient[e] += coefficient;
        }
        let forward = self.forward(entries);
        let outputs = self.outputs();
        // How much the score moves with each first-layer unit's sum.
        let mut through_first = vec![0.0; self.hidden];
        if self.second > 0 {
            for (k, (&sum, &a)) in forward.second_sums.iter().zip(&forward.second_active).enumerate() {
                gradient[outputs + k] += coefficient * a;
                if sum > 0.0 {
                    let through = coefficient * self.params[outputs + k];
                    gradient[self.second_biases() + k] += through;
                    for (unit, &a1) in forward.active.iter().enumerate() {
                        let w = self.second_weights() + unit * self.second + k;
                        gradient[w] += through * a1;
                        through_first[unit] += through * self.params[w];
                    }
                }
            }
        } else {
            for (unit, &a) in forward.active.iter().enumerate() {
                gradient[outputs + unit] += coefficient * a;
                through_first[unit] = coefficient * self.params[outputs + unit];
            }
        }
        for (unit, &sum) in forward.sums.iter().enumerate() {
            if sum > 0.0 {
                let through = through_first[unit];
                gradient[self.first_biases() + unit] += through;
                for &e in entries {
                    gradient[self.first_weights() + e * self.hidden + unit] += through;
                }
            }
        }
    }

    /// Which of `candidates` it scores highest.
    #[cfg(test)]
    pub(crate) fn choice(&self, candidates: &[(Vec<usize>, f64)]) -> usize {
        let scores: Vec<f64> = candidates.iter().map(|(e, _)| self.score(e)).collect();
        let mut best = 0;
        for (i, &score) in scores.iter().enumerate() {
            if score > scores[best] {
                best = i;
            }
        }
        best
    }

    /// Which units of each layer are active (above 0) for a candidate with
    /// `entries`: the first layer's, then the second's.
    #[cfg(test)]
    pub(crate) fn active_units(&self, entries: &[usize]) -> (Vec<bool>, Vec<bool>) {
        let forward = self.forward(entries);
        let active = |values: &[f64]| values.iter().map(|&v| v > 0.0).collect();
        (active(&forward.active), active(&forward.second_active))
    }

    /// The mean value of the candidate it chooses, over `choices`.
    #[cfg(test)]
    pub(crate) fn mean_value(&self, choices: &[&Vec<(Vec<usize>, f64)>]) -> f64 {
        let total: f64 = choices.iter().map(|c| c[self.choice(c)].1).sum();
        total / choices.len() as f64
    }

    /// The parameters, one a line, to be read back with [`Network::read`].
    #[cfg(test)]
    pub(crate) fn write(&self, path: &str) {
        let text: String = self.params.iter().map(|w| format!("{w}\n")).collect();
        std::fs::write(path, text).unwrap();
    }

    /// Multiplies the score of every candidate by `factor`, exactly: the
    /// linear weights and the output weights are multiplied, and the units,
    /// which do not depend on them, are left as they are.
    #[cfg(test)]
    pub(crate) fn scale_scores(&mut self, factor: f64) {
        let outputs = self.outputs();
        let (start, rest) = self.params.split_at_mut(outputs);
        for p in start[..self.entries].iter_mut().chain(rest) {
            *p *= factor;
        }
    }

    /// The network of these sizes written to the file `path`.
    #[cfg(test)]
    pub(crate) fn read(path: &str, entries: usize, hidden: usize, second: usize) -> Network {
        let text = std::fs::read_to_string(path).unwrap();
        Network::with_params(entries, hidden, second, text.lines().map(|l| l.parse().unwrap()).collect())
    }
}

/// How many parameters a network of these sizes has.
pub(crate) fn size(entries: usize, hidden: usize, second: usize) -> usize {
    let last = if second > 0 { second } else { hidden };
    entries * (1 + hidden) + hidden + hidden * second + second + last
}

/// Steps of Adam on a network's parameters, ascending.
#[cfg(test)]
#[derive(Clone)]
pub(crate) struct Adam {
    first: Vec<f64>,
    second: Vec<f64>,
    steps: usize,
}

#[cfg(test)]
impl Adam {
    pub(crate) fn new(size: usize) -> Adam {
        Adam { first: vec![0.0; size], second: vec![0.0; size], steps: 0 }
    }

    /// Its state: the first and second moments of the gradients, and the
    /// steps taken.
    pub(crate) fn parts(&self) -> (&[f64], &[f64], usize) {
        (&self.first, &self.second, self.steps)
    }

    /// Adam with the state that [`Adam::parts`] gives.
    pub(crate) fn from_parts(first: Vec<f64>, second: Vec<f64>, steps: usize) -> Adam {
        Adam { first, second, steps }
    }

    /// Moves `params` a step of size `rate` up `gradient`.
    pub(crate) fn step(&mut self, params: &mut [f64], gradient: &[f64], rate: f64) {
        self.steps += 1;
        let t = self.steps as f64;
        for j in 0..params.len() {
            self.first[j] = 0.9 * self.first[j] + 0.1 * gradient[j];
            self.second[j] = 0.999 * self.second[j] + 0.001 * gradient[j] * gradient[j];
            let a = self.first[j] / (1.0 - 0.9f64.powf(t));
            let b = self.second[j] / (1.0 - 0.999f64.powf(t));
            params[j] += rate * a / (b.sqrt() + 1e-8);
        }
    }
}

/// `network` fitted to `choices` by `steps` steps of Adam, with a step that
/// shrinks to nothing, less `penalty` times the squares of the parameters.
#[cfg(test)]
pub(crate) fn fit(
    choices: &[&Vec<(Vec<usize>, f64)>],
    mut network: Network,
    penalty: f64,
    steps: usize,
) -> Network {
    let mut adam = Adam::new(network.params.len());
    for step in 1..=steps {
        let mut gradient: Vec<f64> = network.params.iter().map(|w| -penalty * w).collect();
        for candidates in choices {
            let scores: Vec<f64> = candidates.iter().map(|(e, _)| network.score(e)).collect();
            let most = scores.iter().copied().fold(f64::MIN, f64::max);
            let odds: Vec<f64> = scores.iter().map(|s| (s - most).exp()).collect();
            let sum: f64 = odds.iter().sum();
            let mean: f64 = odds.iter().zip(*candidates).map(|(p, c)| p * c.1).sum::<f64>() / sum;
            for ((entries, value), p) in candidates.iter().zip(&odds) {
                let g = p / sum * (value - mean) / choices.len() as f64;
                network.add_gradient(entries, g, &mut gradient);
            }
        }
        let rate = 0.05 * (1.0 - (step as f64 - 1.0) / steps as f64);
        adam.step(&mut network.params, &gradient, rate);
    }
    network
}

/// The penalty of `penalties` whose fits of the network that `start` makes
/// choose best on held-out choices, by five-fold cross-validation, the folds
/// running at once, with the mean value held out that each gives.  The
/// choice `i` is in fold `i % 5`.
#[cfg(test)]
pub(crate) fn cross_validate(
    choices: &Choices,
    start: &(dyn Fn() -> Network + Sync),
    penalties: &[f64],
    steps: usize,
) -> (f64, Vec<(f64, f64)>) {
    let mut held_out = Vec::new();
    for &penalty in penalties {
        let total: f64 = std::thread::scope(|scope| {
            let folds: Vec<_> = (0..5)
                .map(|fold| {
                    scope.spawn(move || {
                        let split = |held: bool| -> Vec<&Vec<(Vec<usize>, f64)>> {
                            choices
                                .iter()
                                .enumerate()
                                .filter(|(i, _)| (i % 5 == fold) == held)
                                .map(|(_, c)| c)
                                .collect()
                        };
                        let held = split(true);
                        let network = fit(&split(false), start(), penalty, steps);
                        network.mean_value(&held) * held.len() as f64
                    })
                })
                .collect();
            folds.into_iter().map(|f| f.join().unwrap()).sum()
        });
        held_out.push((penalty, total / choices.len() as f64));
    }
    let best = held_out
        .iter()
        .copied()
        .fold((0.0, f64::MIN), |a, b| if b.1 > a.1 { b } else { a });
    (best.0, held_out)
}

#[test]
fn the_gradient_is_the_slope_of_the_score() {
    for second in [0, 2] {
        the_gradient_is_the_slope_with(second);
    }
}

#[cfg(test)]
fn the_gradient_is_the_slope_with(second: usize) {
    let mut network = Network::new(10, 3, second, 1);
    let mut rng = StdRng::seed_from_u64(2);
    for p in &mut network.params {
        *p += rng.gen_range(-0.5..0.5);
    }
    let entries = [1, 4, 7];
    let mut gradient = vec![0.0; network.params.len()];
    network.add_gradient(&entries, 1.0, &mut gradient);
    for j in 0..network.params.len() {
        let (mut up, mut down) = (network.clone(), network.clone());
        up.params[j] += 1e-6;
        down.params[j] -= 1e-6;
        let slope = (up.score(&entries) - down.score(&entries)) / 2e-6;
        assert!((slope - gradient[j]).abs() < 1e-4, "parameter {j}: {slope} and {}", gradient[j]);
    }
}

#[test]
fn scaling_the_scores_multiplies_every_score() {
    for second in [0, 2] {
        let mut network = Network::new(10, 3, second, 1);
        let mut rng = StdRng::seed_from_u64(3);
        for p in &mut network.params {
            *p += rng.gen_range(-0.5..0.5);
        }
        let mut scaled = network.clone();
        scaled.scale_scores(0.3);
        for entries in [[1, 4, 7], [0, 2, 9], [3, 5, 8]] {
            let (score, now) = (network.score(&entries), scaled.score(&entries));
            assert!((now - 0.3 * score).abs() < 1e-12, "{entries:?}: {now} for {score}");
        }
    }
}
