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
pub(crate) type Choices<E = Vec<usize>> = Vec<Vec<(E, f64)>>;

/// What a candidate looks up in a network's tables: a list of entries, or
/// [`Entries`] with some of them weighted.
#[cfg(test)]
pub(crate) trait Lookups: Sync {
    /// The score of a candidate that looks these up.
    fn score_in(&self, network: &Network) -> f64;
    /// Adds `coefficient` times the gradient of that score to `gradient`.
    fn add_gradient_in(&self, network: &Network, coefficient: f64, gradient: &mut [f64]);
}

#[cfg(test)]
impl Lookups for Vec<usize> {
    fn score_in(&self, network: &Network) -> f64 {
        network.score(self)
    }
    fn add_gradient_in(&self, network: &Network, coefficient: f64, gradient: &mut [f64]) {
        network.add_gradient(self, coefficient, gradient);
    }
}

#[cfg(test)]
impl Lookups for Entries {
    fn score_in(&self, network: &Network) -> f64 {
        network.score_of(self)
    }
    fn add_gradient_in(&self, network: &Network, coefficient: f64, gradient: &mut [f64]) {
        network.add_gradient_of(self, coefficient, gradient);
    }
}

/// The entries of a network's tables that a candidate looks up: entries
/// that are there, and entries that are there with a chance, such as those
/// for where cards that cannot be seen might lie, each with that chance.
#[derive(Clone, Debug, Default, PartialEq)]
pub(crate) struct Entries {
    pub(crate) one: Vec<usize>,
    pub(crate) weighted: Vec<(usize, f64)>,
    /// Which of the network's heads scores it (see [`Network`]).
    pub(crate) head: usize,
}

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
///
/// It may have several heads, each with its own layers after the first and
/// its own output weights, which share the entries' weights and the first
/// layer: a candidate is scored by the head that its [`Entries`] name, so
/// that kinds of situation (declarer and the defence, say) can each weigh
/// the same first units their own way.
#[derive(Clone)]
pub(crate) struct Network {
    entries: usize,
    /// How many units each layer has, the first first.
    layers: Vec<usize>,
    /// How many heads it has.
    heads: usize,
    /// The linear weights, one an entry; then the first layer's weights, as
    /// many an entry as it has units, and its biases; then for each head,
    /// each later layer's weights, as many for each unit of the layer before
    /// as it has units, and its biases, and then the output weights of the
    /// last layer.
    pub(crate) params: Vec<f64>,
}

/// What a network works out on the way to a candidate's score.
struct Forward {
    /// Each layer's sums, and the larger of each and 0.
    #[cfg_attr(not(test), expect(dead_code, reason = "only fitting reads the sums"))]
    sums: Vec<Vec<f64>>,
    active: Vec<Vec<f64>>,
}

impl Network {
    /// How many parameters a network of these sizes has.
    pub(crate) fn size(entries: usize, layers: &[usize], heads: usize) -> usize {
        size(entries, layers, heads)
    }

    /// A network of `entries` entries and `layers` of units (none for the
    /// sum of weights alone), that scores every candidate the same, so that
    /// it starts by choosing at random.  The units' weights are small and
    /// random, the same for the same `seed`, so that the units can learn
    /// different things.
    #[cfg(test)]
    pub(crate) fn new(entries: usize, layers: &[usize], seed: u64) -> Network {
        let mut rng = StdRng::seed_from_u64(seed);
        let mut network = Network { entries, layers: layers.to_vec(), heads: 1, params: vec![0.0; size(entries, layers, 1)] };
        for layer in 0..layers.len() {
            let (start, end) = (network.weights(layer, 0), network.biases(layer, 0));
            let range = if layer == 0 { 0.1 } else { 0.5 };
            for p in &mut network.params[start..end] {
                *p = rng.gen_range(-range..range);
            }
        }
        network
    }

    /// The network of these sizes with `params`, such as one written out
    /// before.
    pub(crate) fn with_params(entries: usize, layers: &[usize], heads: usize, params: Vec<f64>) -> Network {
        assert_eq!(params.len(), size(entries, layers, heads), "the size of the network");
        Network { entries, layers: layers.to_vec(), heads, params }
    }

    /// This network of one head with `heads` heads, each a copy of its
    /// layers after the first, so that it scores every candidate as it did
    /// whatever head scores it.
    #[cfg(test)]
    pub(crate) fn with_heads(&self, heads: usize) -> Network {
        assert_eq!(self.heads, 1, "only a network of one head grows heads");
        let shared = self.head_start(0);
        let mut params = self.params[..shared].to_vec();
        for _ in 0..heads {
            params.extend_from_slice(&self.params[shared..]);
        }
        Network { entries: self.entries, layers: self.layers.clone(), heads, params }
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
        let new = Network { entries, layers: self.layers.clone(), heads: self.heads, params: Vec::new() };
        let mut result = vec![0.0; size(entries, &self.layers, self.heads)];
        for (old, to) in moved.iter().enumerate() {
            if let Some(to) = *to {
                result[to] = values[old];
                let (from, to) = (self.weights(0, 0) + old * h, new.weights(0, 0) + to * h);
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
        assert_eq!(self.heads, 1, "only a network of one head grows (see Network::with_heads)");
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
                let row = grown.weights(0, 0) + entry * h;
                grown.params[row..row + old_h].fill(0.0);
            }
            for (o, &n) in moved.iter().enumerate() {
                let (from, to) = (self.weights(0, 0) + o * old_h, grown.weights(0, 0) + n * h);
                grown.params[to..to + old_h].copy_from_slice(&self.params[from..from + old_h]);
            }
            if old_h > 0 {
                let (from, to) = (self.biases(0, 0), grown.biases(0, 0));
                grown.params[to..to + old_h].copy_from_slice(&self.params[from..from + old_h]);
            }
        }
        let last = old.last().copied().unwrap_or(0);
        for layer in 1..layers.len() {
            let (inputs, width) = (layers[layer - 1], layers[layer]);
            if let Some(&old_width) = old.get(layer) {
                let old_inputs = old[layer - 1];
                for unit in 0..inputs {
                    let to = grown.weights(layer, 0) + unit * width;
                    if unit < old_inputs {
                        let from = self.weights(layer, 0) + unit * old_width;
                        grown.params[to..to + old_width].copy_from_slice(&self.params[from..from + old_width]);
                    } else {
                        grown.params[to..to + old_width].fill(0.0);
                    }
                }
                let (from, to) = (self.biases(layer, 0), grown.biases(layer, 0));
                grown.params[to..to + old_width].copy_from_slice(&self.params[from..from + old_width]);
            } else {
                // An added layer: its first `last` units pass on those of
                // the layer before.
                for unit in 0..inputs {
                    let to = grown.weights(layer, 0) + unit * width;
                    grown.params[to..to + last].fill(0.0);
                    if unit < last {
                        grown.params[to + unit] = 1.0;
                    }
                }
            }
        }
        // The output weights of the last layer: the old units', and 0 for
        // the new ones.
        let (from, to) = (self.outputs(0), grown.outputs(0));
        grown.params[to..].fill(0.0);
        grown.params[to..to + last].copy_from_slice(&self.params[from..from + last]);
        grown
    }

    /// Gives entry `to` the weights of entry `from` times `factor`: its
    /// linear weight and its weights into the first layer.
    #[cfg(test)]
    pub(crate) fn copy_entry(&mut self, from: usize, to: usize, factor: f64) {
        self.params[to] = factor * self.params[from];
        let h = self.layers.first().copied().unwrap_or(0);
        let (from, to) = (self.weights(0, 0) + from * h, self.weights(0, 0) + to * h);
        for k in 0..h {
            self.params[to + k] = factor * self.params[from + k];
        }
    }

    /// Where the weights into `layer` of `head` start (every head shares
    /// the first layer).
    fn weights(&self, layer: usize, head: usize) -> usize {
        match layer {
            0 => self.entries,
            1 => self.head_start(head),
            _ => self.biases(layer - 1, head) + self.layers[layer - 1],
        }
    }
    /// Where the biases of `layer` of `head` start.
    fn biases(&self, layer: usize, head: usize) -> usize {
        let inputs = if layer == 0 { self.entries } else { self.layers[layer - 1] };
        self.weights(layer, head) + inputs * self.layers[layer]
    }
    /// Where the weights of `head` start, after those that every head shares.
    fn head_start(&self, head: usize) -> usize {
        match self.layers.first() {
            None => self.entries,
            Some(&h) => self.entries * (1 + h) + h + head * self.head_size(),
        }
    }
    /// How many weights each head has of its own.
    fn head_size(&self) -> usize {
        let later: usize = self.layers.windows(2).map(|w| w[0] * w[1] + w[1]).sum();
        later + self.layers.last().copied().unwrap_or(0)
    }
    /// Where the weights that do not belong to an entry start.
    #[cfg(test)]
    fn after_entries(&self) -> usize {
        self.entries * (1 + self.layers.first().copied().unwrap_or(0))
    }
    /// Where the output weights of `head` start.
    fn outputs(&self, head: usize) -> usize {
        match self.layers.len() {
            0 => self.entries,
            1 => self.head_start(head),
            n => self.biases(n - 1, head) + self.layers[n - 1],
        }
    }

    /// The layers' sums and outputs for a candidate with `entries`.
    #[cfg(test)]
    fn forward(&self, entries: &[usize]) -> Forward {
        self.forward_with(entries, &[], 0)
    }

    /// [`Network::forward`] of `one` entries and `weighted` ones, each
    /// counted its weight times, by `head`.
    fn forward_with(&self, one: &[usize], weighted: &[(usize, f64)], head: usize) -> Forward {
        let mut sums: Vec<Vec<f64>> = Vec::with_capacity(self.layers.len());
        let mut active: Vec<Vec<f64>> = Vec::with_capacity(self.layers.len());
        for (layer, &width) in self.layers.iter().enumerate() {
            let biases = self.biases(layer, head);
            let mut sum = self.params[biases..biases + width].to_vec();
            let weights = self.weights(layer, head);
            let mut add = |input: usize, a: f64| {
                let row = weights + input * width;
                for (s, w) in sum.iter_mut().zip(&self.params[row..row + width]) {
                    *s += w * a;
                }
            };
            match active.last() {
                None => {
                    one.iter().for_each(|&e| add(e, 1.0));
                    weighted.iter().for_each(|&(e, w)| add(e, w));
                }
                Some(before) => before.iter().enumerate().filter(|&(_, &a)| a != 0.0).for_each(|(i, &a)| add(i, a)),
            }
            active.push(sum.iter().map(|s| s.max(0.0)).collect());
            sums.push(sum);
        }
        Forward { sums, active }
    }

    /// The score of a candidate with `entries`.
    #[cfg(test)]
    pub(crate) fn score(&self, entries: &[usize]) -> f64 {
        self.score_with(entries, &[], 0)
    }

    /// The score of a candidate with `entries`.
    pub(crate) fn score_of(&self, entries: &Entries) -> f64 {
        self.score_with(&entries.one, &entries.weighted, entries.head)
    }

    /// The score of a candidate with `one` entries and `weighted` ones: the
    /// expected score, for the linear weights, of a candidate whose
    /// weighted entries are each there with their weight as its chance, and
    /// the score of their mean for the units, by `head`.
    fn score_with(&self, one: &[usize], weighted: &[(usize, f64)], head: usize) -> f64 {
        debug_assert!(head < self.heads, "head {head} of {}", self.heads);
        let linear: f64 =
            one.iter().map(|&e| self.params[e]).sum::<f64>() + weighted.iter().map(|&(e, w)| w * self.params[e]).sum::<f64>();
        let forward = self.forward_with(one, weighted, head);
        let outputs = &self.params[self.outputs(head)..];
        let units: f64 = forward.active.last().map_or(0.0, |last| last.iter().zip(outputs).map(|(a, o)| a * o).sum());
        linear + units
    }

    /// Adds `coefficient` times the gradient of the score of a candidate with
    /// `entries` to `gradient`.
    #[cfg(test)]
    pub(crate) fn add_gradient(&self, entries: &[usize], coefficient: f64, gradient: &mut [f64]) {
        self.add_gradient_with(entries, &[], 0, coefficient, gradient);
    }

    /// [`Network::add_gradient`] of a candidate with `entries`.
    #[cfg(test)]
    pub(crate) fn add_gradient_of(&self, entries: &Entries, coefficient: f64, gradient: &mut [f64]) {
        self.add_gradient_with(&entries.one, &entries.weighted, entries.head, coefficient, gradient);
    }

    #[cfg(test)]
    fn add_gradient_with(&self, one: &[usize], weighted: &[(usize, f64)], head: usize, coefficient: f64, gradient: &mut [f64]) {
        for &e in one {
            gradient[e] += coefficient;
        }
        for &(e, w) in weighted {
            gradient[e] += coefficient * w;
        }
        let Some(&last) = self.layers.last() else { return };
        let forward = self.forward_with(one, weighted, head);
        let outputs = self.outputs(head);
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
            let biases = self.biases(layer, head);
            for (k, &t) in into.iter().enumerate() {
                gradient[biases + k] += t;
            }
            let weights = self.weights(layer, head);
            if layer == 0 {
                for &e in one {
                    for (k, &t) in into.iter().enumerate() {
                        gradient[weights + e * width + k] += t;
                    }
                }
                for &(e, w) in weighted {
                    for (k, &t) in into.iter().enumerate() {
                        gradient[weights + e * width + k] += t * w;
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
    pub(crate) fn choice<E: Lookups>(&self, candidates: &[(E, f64)]) -> usize {
        let scores: Vec<f64> = candidates.iter().map(|(e, _)| e.score_in(self)).collect();
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
    pub(crate) fn mean_value<E: Lookups>(&self, choices: &[&Vec<(E, f64)>]) -> f64 {
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
    /// linear weights and each head's output weights are multiplied, and the
    /// units, which do not depend on them, are left as they are.
    #[cfg(test)]
    pub(crate) fn scale_scores(&mut self, factor: f64) {
        let last = self.layers.last().copied().unwrap_or(0);
        let outputs: Vec<usize> = (0..self.heads).map(|head| self.outputs(head)).collect();
        for p in &mut self.params[..self.entries] {
            *p *= factor;
        }
        for start in outputs {
            for p in &mut self.params[start..start + last] {
                *p *= factor;
            }
        }
    }

    /// The network of these sizes written to the file `path`.
    #[cfg(test)]
    pub(crate) fn read(path: &str, entries: usize, layers: &[usize], heads: usize) -> Network {
        let text = std::fs::read_to_string(path).unwrap();
        Network::with_params(entries, layers, heads, text.lines().map(|l| l.parse().unwrap()).collect())
    }
}

/// How many parameters a network of these sizes has.
pub(crate) fn size(entries: usize, layers: &[usize], heads: usize) -> usize {
    let network = Network { entries, layers: layers.to_vec(), heads, params: Vec::new() };
    network.head_start(heads)
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
pub(crate) fn fit<E: Lookups>(
    choices: &[&Vec<(E, f64)>],
    mut network: Network,
    penalty: f64,
    steps: usize,
) -> Network {
    let mut adam = Adam::new(network.params.len());
    for step in 1..=steps {
        let mut gradient: Vec<f64> = network.params.iter().map(|w| -penalty * w).collect();
        for candidates in choices {
            let scores: Vec<f64> = candidates.iter().map(|(e, _)| e.score_in(&network)).collect();
            let most = scores.iter().copied().fold(f64::MIN, f64::max);
            let odds: Vec<f64> = scores.iter().map(|s| (s - most).exp()).collect();
            let sum: f64 = odds.iter().sum();
            let mean: f64 = odds.iter().zip(*candidates).map(|(p, c)| p * c.1).sum::<f64>() / sum;
            for ((entries, value), p) in candidates.iter().zip(&odds) {
                let g = p / sum * (value - mean) / choices.len() as f64;
                entries.add_gradient_in(&network, g, &mut gradient);
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
pub(crate) fn cross_validate<E: Lookups>(
    choices: &Choices<E>,
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
                        let split = |held: bool| -> Vec<&Vec<(E, f64)>> {
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
    for (heads, head) in [(1, 0), (3, 0), (3, 2)] {
        let mut network = Network::new(10, layers, 1).with_heads(heads);
        let mut rng = StdRng::seed_from_u64(2);
        for p in &mut network.params {
            *p += rng.gen_range(-0.5..0.5);
        }
        let entries = Entries { one: vec![1, 4, 7], weighted: vec![(2, 0.25)], head };
        let mut gradient = vec![0.0; network.params.len()];
        network.add_gradient_of(&entries, 1.0, &mut gradient);
        for j in 0..network.params.len() {
            let (mut up, mut down) = (network.clone(), network.clone());
            up.params[j] += 1e-6;
            down.params[j] -= 1e-6;
            let slope = (up.score_of(&entries) - down.score_of(&entries)) / 2e-6;
            assert!(
                (slope - gradient[j]).abs() < 1e-4,
                "{layers:?}, head {head} of {heads}, parameter {j}: {slope} and {}",
                gradient[j]
            );
        }
    }
}

/// A network of one head given more scores every candidate as it did,
/// whatever head scores it, and each head then learns apart from the
/// others: a candidate's gradient reaches none of the other heads' weights.
#[test]
fn a_network_given_heads_scores_as_it_did() {
    for layers in TRIED {
        let mut network = Network::new(10, layers, 1);
        let mut rng = StdRng::seed_from_u64(6);
        for p in &mut network.params {
            *p += rng.gen_range(-0.5..0.5);
        }
        let headed = network.with_heads(3);
        assert_eq!(headed.params.len(), Network::size(10, layers, 3));
        for _ in 0..20 {
            let one: Vec<usize> = (0..3).map(|_| rng.gen_range(0..10)).collect();
            let before = network.score(&one);
            for head in 0..3 {
                let entries = Entries { one: one.clone(), weighted: Vec::new(), head };
                let after = headed.score_of(&entries);
                assert!((before - after).abs() < 1e-12, "{layers:?}, head {head}: {before} and {after}");
                let mut gradient = vec![0.0; headed.params.len()];
                headed.add_gradient_of(&entries, 1.0, &mut gradient);
                for other in (0..3).filter(|&h| h != head) {
                    let start = headed.head_start(other);
                    assert!(gradient[start..start + headed.head_size()].iter().all(|&g| g == 0.0), "{layers:?}");
                }
            }
        }
    }
}

#[test]
fn scaling_the_scores_multiplies_every_score() {
    for layers in TRIED {
        let mut network = Network::new(10, layers, 1).with_heads(2);
        let mut rng = StdRng::seed_from_u64(3);
        for p in &mut network.params {
            *p += rng.gen_range(-0.5..0.5);
        }
        let mut scaled = network.clone();
        scaled.scale_scores(0.3);
        for (one, head) in [(vec![1, 4, 7], 0), (vec![0, 2, 9], 1), (vec![3, 5, 8], 1)] {
            let entries = Entries { one, weighted: Vec::new(), head };
            let (score, now) = (network.score_of(&entries), scaled.score_of(&entries));
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
        let outputs = &gradient[grown.outputs(0)..];
        assert!(outputs.iter().any(|&g| g != 0.0), "{old:?} to {new:?}: {outputs:?}");
    }
    assert!(!grows(&[3, 2], &[3, 1, 2]) && !grows(&[3, 2], &[3]) && !grows(&[3, 2], &[3, 2, 1]));
}
