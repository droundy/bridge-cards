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

/// An n-tuple network with layers of hidden units.  The score of a
/// candidate is the sum of the linear weights of its entries, plus the
/// output of the units: each unit of the first layer sums its weights of the
/// candidate's entries and a bias, as an NNUE accumulator is summed, a few
/// additions an entry, and takes the larger of that and 0; each unit of a
/// later layer does the same with its weights of the units of the layer
/// before; and the last layer's units, times their output weights, are added
/// to the score.  The units let the network weigh combinations of entries
/// from different tables, and each later layer combinations of those.  With
/// no units it is the sum of weights alone.
#[derive(Clone)]
pub(crate) struct Network {
    entries: usize,
    /// How many units each layer has, the first first.
    layers: Vec<usize>,
    /// The linear weights, one an entry; then the first layer's weights, as
    /// many an entry as it has units, and its biases; then each later
    /// layer's weights, as many for each unit of the layer before as it has
    /// units, and its biases; then the output weights of the last layer.
    pub(crate) params: Vec<f64>,
}

/// What a network works out on the way to a candidate's score.
#[cfg(test)]
struct Forward {
    /// Each layer's sums, and the larger of each and 0.
    sums: Vec<Vec<f64>>,
    active: Vec<Vec<f64>>,
}

impl Network {
    /// How many parameters a network of these sizes has.
    pub(crate) fn size(entries: usize, layers: &[usize]) -> usize {
        size(entries, layers)
    }

    /// A network of `entries` entries and `layers` of units (none for the
    /// sum of weights alone), that scores every candidate the same, so that
    /// it starts by choosing at random.  The units' weights are small and
    /// random, the same for the same `seed`, so that the units can learn
    /// different things.
    #[cfg(test)]
    pub(crate) fn new(entries: usize, layers: &[usize], seed: u64) -> Network {
        let mut rng = StdRng::seed_from_u64(seed);
        let mut network = Network { entries, layers: layers.to_vec(), params: vec![0.0; size(entries, layers)] };
        for layer in 0..layers.len() {
            let (start, end) = (network.weights(layer), network.biases(layer));
            let range = if layer == 0 { 0.1 } else { 0.5 };
            for p in &mut network.params[start..end] {
                *p = rng.gen_range(-range..range);
            }
        }
        network
    }

    /// The network of these sizes with `params`, such as one written out
    /// before.
    pub(crate) fn with_params(entries: usize, layers: &[usize], params: Vec<f64>) -> Network {
        assert_eq!(params.len(), size(entries, layers), "the size of the network");
        Network { entries, layers: layers.to_vec(), params }
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
        let h = self.layers.first().copied().unwrap_or(0);
        let new = Network { entries, layers: self.layers.clone(), params: Vec::new() };
        let mut result = vec![0.0; size(entries, &self.layers)];
        for (old, to) in moved.iter().enumerate() {
            if let Some(to) = *to {
                result[to] = values[old];
                let (from, to) = (self.weights(0) + old * h, new.weights(0) + to * h);
                result[to..to + h].copy_from_slice(&values[from..from + h]);
            }
        }
        let start = new.after_entries();
        result[start..].copy_from_slice(&values[self.after_entries()..]);
        result
    }

    /// This network grown into one of `entries` entries and `layers` of
    /// units (see [`grows`]), which scores every candidate as it did: entry
    /// `e` becomes `moved[e]`, with its weights, and the entries and units
    /// that are new add nothing.  An entry or unit that is new has weights
    /// only into the new units of the layer after it, and a new unit of the
    /// last layer has an output weight of 0, so that none changes a score;
    /// the weights into the new units are the small random ones of
    /// [`Network::new`] with `seed`, so that they can learn.  A layer that is
    /// added after the last one passes on each of that layer's units, as its
    /// first units: a weight of 1 from the unit of the same number and 0 from
    /// the others, which changes nothing because the units are never below
    /// 0.
    #[cfg(test)]
    pub(crate) fn grown(&self, moved: &[usize], entries: usize, layers: &[usize], seed: u64) -> Network {
        assert_eq!(moved.len(), self.entries);
        assert!(grows(&self.layers, layers), "{:?} cannot grow into {layers:?}", self.layers);
        let old = &self.layers;
        let mut grown = Network::new(entries, layers, seed);
        for (o, &n) in moved.iter().enumerate() {
            grown.params[n] = self.params[o];
        }
        if let Some(&h) = layers.first() {
            let old_h = old.first().copied().unwrap_or(0);
            // Every new entry's weights into the old units are 0.
            for entry in 0..entries {
                let row = grown.weights(0) + entry * h;
                grown.params[row..row + old_h].fill(0.0);
            }
            for (o, &n) in moved.iter().enumerate() {
                let (from, to) = (self.weights(0) + o * old_h, grown.weights(0) + n * h);
                grown.params[to..to + old_h].copy_from_slice(&self.params[from..from + old_h]);
            }
            if old_h > 0 {
                let (from, to) = (self.biases(0), grown.biases(0));
                grown.params[to..to + old_h].copy_from_slice(&self.params[from..from + old_h]);
            }
        }
        let last = old.last().copied().unwrap_or(0);
        for layer in 1..layers.len() {
            let (inputs, width) = (layers[layer - 1], layers[layer]);
            if let Some(&old_width) = old.get(layer) {
                let old_inputs = old[layer - 1];
                for unit in 0..inputs {
                    let to = grown.weights(layer) + unit * width;
                    if unit < old_inputs {
                        let from = self.weights(layer) + unit * old_width;
                        grown.params[to..to + old_width].copy_from_slice(&self.params[from..from + old_width]);
                    } else {
                        grown.params[to..to + old_width].fill(0.0);
                    }
                }
                let (from, to) = (self.biases(layer), grown.biases(layer));
                grown.params[to..to + old_width].copy_from_slice(&self.params[from..from + old_width]);
            } else {
                // An added layer: its first `last` units pass on those of
                // the layer before.
                for unit in 0..inputs {
                    let to = grown.weights(layer) + unit * width;
                    grown.params[to..to + last].fill(0.0);
                    if unit < last {
                        grown.params[to + unit] = 1.0;
                    }
                }
            }
        }
        // The output weights of the last layer: the old units', and 0 for
        // the new ones.
        let (from, to) = (self.outputs(), grown.outputs());
        grown.params[to..].fill(0.0);
        grown.params[to..to + last].copy_from_slice(&self.params[from..from + last]);
        grown
    }

    /// Where the weights into `layer` start.
    fn weights(&self, layer: usize) -> usize {
        if layer == 0 { self.entries } else { self.biases(layer - 1) + self.layers[layer - 1] }
    }
    /// Where the biases of `layer` start.
    fn biases(&self, layer: usize) -> usize {
        let inputs = if layer == 0 { self.entries } else { self.layers[layer - 1] };
        self.weights(layer) + inputs * self.layers[layer]
    }
    /// Where the weights that do not belong to an entry start.
    #[cfg(test)]
    fn after_entries(&self) -> usize {
        self.entries * (1 + self.layers.first().copied().unwrap_or(0))
    }
    fn outputs(&self) -> usize {
        match self.layers.len() {
            0 => self.entries,
            n => self.biases(n - 1) + self.layers[n - 1],
        }
    }

    /// The layers' sums and outputs for a candidate with `entries`.
    #[cfg(test)]
    fn forward(&self, entries: &[usize]) -> Forward {
        let mut sums: Vec<Vec<f64>> = Vec::with_capacity(self.layers.len());
        let mut active: Vec<Vec<f64>> = Vec::with_capacity(self.layers.len());
        for (layer, &width) in self.layers.iter().enumerate() {
            let biases = self.biases(layer);
            let mut sum = self.params[biases..biases + width].to_vec();
            let weights = self.weights(layer);
            let mut add = |input: usize, a: f64| {
                let row = weights + input * width;
                for (s, w) in sum.iter_mut().zip(&self.params[row..row + width]) {
                    *s += w * a;
                }
            };
            match active.last() {
                None => entries.iter().for_each(|&e| add(e, 1.0)),
                Some(before) => before.iter().enumerate().filter(|&(_, &a)| a != 0.0).for_each(|(i, &a)| add(i, a)),
            }
            active.push(sum.iter().map(|s| s.max(0.0)).collect());
            sums.push(sum);
        }
        Forward { sums, active }
    }

    /// The score of a candidate with `entries`: what [`Network::forward`]
    /// works out, with the layers' sums kept on the stack, since this is
    /// asked for every card of every position that a game plays.  The sums
    /// are added in the same order, so that the score is the same to the last
    /// bit (see `score_by_forward`).
    pub(crate) fn score(&self, entries: &[usize]) -> f64 {
        /// The widest layer that fits.
        const MOST: usize = 64;
        let linear: f64 = entries.iter().map(|&e| self.params[e]).sum();
        let mut before = [0.0; MOST];
        let mut width_before = 0;
        for (layer, &width) in self.layers.iter().enumerate() {
            assert!(width <= MOST, "a layer of {width} units");
            let biases = self.biases(layer);
            let mut sum = [0.0; MOST];
            sum[..width].copy_from_slice(&self.params[biases..biases + width]);
            let weights = self.weights(layer);
            let mut add = |input: usize, a: f64| {
                let row = weights + input * width;
                for (s, w) in sum[..width].iter_mut().zip(&self.params[row..row + width]) {
                    *s += w * a;
                }
            };
            if layer == 0 {
                entries.iter().for_each(|&e| add(e, 1.0));
            } else {
                before[..width_before].iter().enumerate().filter(|&(_, &a)| a != 0.0).for_each(|(i, &a)| add(i, a));
            }
            for (b, s) in before[..width].iter_mut().zip(&sum[..width]) {
                *b = s.max(0.0);
            }
            width_before = width;
        }
        let outputs = &self.params[self.outputs()..];
        let units: f64 =
            if width_before == 0 { 0.0 } else { before[..width_before].iter().zip(outputs).map(|(a, o)| a * o).sum() };
        linear + units
    }

    /// [`Network::score`] as it was, by [`Network::forward`].
    #[cfg(test)]
    fn score_by_forward(&self, entries: &[usize]) -> f64 {
        let linear: f64 = entries.iter().map(|&e| self.params[e]).sum();
        let forward = self.forward(entries);
        let outputs = &self.params[self.outputs()..];
        let units: f64 = forward.active.last().map_or(0.0, |last| last.iter().zip(outputs).map(|(a, o)| a * o).sum());
        linear + units
    }

    /// Adds `coefficient` times the gradient of the score of a candidate with
    /// `entries` to `gradient`.
    #[cfg(test)]
    pub(crate) fn add_gradient(&self, entries: &[usize], coefficient: f64, gradient: &mut [f64]) {
        for &e in entries {
            gradient[e] += coefficient;
        }
        let Some(&last) = self.layers.last() else { return };
        let forward = self.forward(entries);
        let outputs = self.outputs();
        for (k, &a) in forward.active[self.layers.len() - 1].iter().enumerate() {
            gradient[outputs + k] += coefficient * a;
        }
        // How much the score moves with each unit's output of the layer.
        let mut through: Vec<f64> = (0..last).map(|k| coefficient * self.params[outputs + k]).collect();
        for layer in (0..self.layers.len()).rev() {
            let width = self.layers[layer];
            // And with each unit's sum.
            let into: Vec<f64> =
                through.iter().zip(&forward.sums[layer]).map(|(&t, &sum)| if sum > 0.0 { t } else { 0.0 }).collect();
            let biases = self.biases(layer);
            for (k, &t) in into.iter().enumerate() {
                gradient[biases + k] += t;
            }
            let weights = self.weights(layer);
            if layer == 0 {
                for &e in entries {
                    for (k, &t) in into.iter().enumerate() {
                        gradient[weights + e * width + k] += t;
                    }
                }
            } else {
                let mut before = vec![0.0; self.layers[layer - 1]];
                for (unit, (&a, b)) in forward.active[layer - 1].iter().zip(&mut before).enumerate() {
                    for (k, &t) in into.iter().enumerate() {
                        let w = weights + unit * width + k;
                        gradient[w] += t * a;
                        *b += t * self.params[w];
                    }
                }
                through = before;
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
    /// `entries`, the first layer first.
    #[cfg(test)]
    pub(crate) fn active_units(&self, entries: &[usize]) -> Vec<Vec<bool>> {
        self.forward(entries).active.iter().map(|layer| layer.iter().map(|&v| v > 0.0).collect()).collect()
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
    pub(crate) fn read(path: &str, entries: usize, layers: &[usize]) -> Network {
        let text = std::fs::read_to_string(path).unwrap();
        Network::with_params(entries, layers, text.lines().map(|l| l.parse().unwrap()).collect())
    }
}

/// How many parameters a network of these sizes has.
pub(crate) fn size(entries: usize, layers: &[usize]) -> usize {
    let network = Network { entries, layers: layers.to_vec(), params: Vec::new() };
    network.outputs() + layers.last().copied().unwrap_or(0)
}

/// Whether a network with `old` layers of units can grow into one with
/// `new` ([`Network::grown`]): each of its layers is at least as wide, and
/// each layer added after its last is at least as wide as that, so that it
/// can pass that layer on.
#[cfg(test)]
pub(crate) fn grows(old: &[usize], new: &[usize]) -> bool {
    let last = old.last().copied().unwrap_or(0);
    new.len() >= old.len() && old.iter().zip(new).all(|(o, n)| n >= o) && new[old.len()..].iter().all(|&n| n >= last)
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

/// Layers of units of the networks that the tests try.
#[cfg(test)]
const TRIED: [&[usize]; 4] = [&[], &[3], &[3, 2], &[3, 2, 2]];

#[test]
fn the_gradient_is_the_slope_of_the_score() {
    for layers in TRIED {
        the_gradient_is_the_slope_with(layers);
    }
}

#[cfg(test)]
fn the_gradient_is_the_slope_with(layers: &[usize]) {
    let mut network = Network::new(10, layers, 1);
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
        assert!((slope - gradient[j]).abs() < 1e-4, "{layers:?}, parameter {j}: {slope} and {}", gradient[j]);
    }
}

#[test]
fn scaling_the_scores_multiplies_every_score() {
    for layers in TRIED {
        let mut network = Network::new(10, layers, 1);
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

/// A network grown wider, deeper and with more entries scores as it did,
/// and its new units learn: some of the gradient reaches every weight into
/// them.
#[test]
fn a_grown_network_scores_as_it_did() {
    for (old, new) in [(&[][..], &[2][..]), (&[3], &[3, 3]), (&[3, 2], &[4, 2]), (&[3, 2], &[3, 2, 2]), (&[3, 2], &[4, 3, 4, 3])] {
        assert!(grows(old, new));
        let mut network = Network::new(10, old, 1);
        let mut rng = StdRng::seed_from_u64(4);
        for p in &mut network.params {
            *p += rng.gen_range(-0.5..0.5);
        }
        let moved: Vec<usize> = (0..10).map(|e| e + e / 4).collect();
        let grown = network.grown(&moved, 13, new, 5);
        let mut gradient = vec![0.0; grown.params.len()];
        for _ in 0..50 {
            let entries: Vec<usize> = (0..3).map(|_| rng.gen_range(0..10)).collect();
            let at: Vec<usize> = entries.iter().map(|&e| moved[e]).collect();
            let (before, after) = (network.score(&entries), grown.score(&at));
            assert!((before - after).abs() < 1e-12, "{old:?} to {new:?}: {before} and {after}");
            grown.add_gradient(&at, 1.0, &mut gradient);
        }
        let outputs = &gradient[grown.outputs()..];
        assert!(outputs.iter().any(|&g| g != 0.0), "{old:?} to {new:?}: {outputs:?}");
    }
    assert!(!grows(&[3, 2], &[3, 1, 2]) && !grows(&[3, 2], &[3]) && !grows(&[3, 2], &[3, 2, 1]));
}

#[test]
fn the_score_is_what_the_forward_pass_gives_to_the_last_bit() {
    let mut rng = StdRng::seed_from_u64(7);
    for layers in [&[][..], &[4][..], &[16, 8][..], &[32, 16][..], &[16, 8, 8][..]] {
        let mut network = Network::new(40, layers, 5);
        for p in &mut network.params {
            *p += rng.gen_range(-0.5..0.5);
        }
        for _ in 0..200 {
            let entries: Vec<usize> = (0..rng.gen_range(1..14)).map(|_| rng.gen_range(0..40)).collect();
            assert_eq!(
                network.score(&entries).to_bits(),
                network.score_by_forward(&entries).to_bits(),
                "{layers:?}, {entries:?}"
            );
        }
    }
}
