//! The fit that the robot's n-tuple networks share, for the tests that fit
//! them.  A network scores each candidate of a choice, such as each card that
//! could be played, from the table entries that the candidate looks up: the
//! sum of a weight for each entry, plus, with hidden units, a layer of them
//! that each entry also feeds (see [`Network`]).  Fitting makes the
//! candidates of most value the likeliest, where a candidate is chosen at
//! random with odds that grow exponentially with its score, by gradient
//! ascent on the mean value of that choice.

use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

/// For each choice, each candidate's entries and its value to the one who
/// chooses, the more the better.
pub(crate) type Choices = Vec<Vec<(Vec<usize>, f64)>>;

/// An n-tuple network with a layer of hidden units: the score of a
/// candidate is the sum of the linear weights of its entries, plus, for each
/// hidden unit, its output weight times the larger of 0 and its bias plus
/// the unit's weights of the candidate's entries.  The units are summed from
/// the entries as an NNUE accumulator is, so they cost a few additions an
/// entry, and they let the network weigh combinations of entries from
/// different tables.  With no hidden units it is the sum of weights alone.
#[derive(Clone)]
pub(crate) struct Network {
    entries: usize,
    hidden: usize,
    /// The linear weights, one an entry; then the units' weights, `hidden`
    /// an entry; then the units' biases; then their output weights.
    pub(crate) params: Vec<f64>,
}

impl Network {
    /// A network of `entries` entries and `hidden` units that scores every
    /// candidate the same, so that it starts by choosing at random.  The
    /// units' weights are small and random, the same for the same `seed`,
    /// so that the units can learn different things.
    pub(crate) fn new(entries: usize, hidden: usize, seed: u64) -> Network {
        let mut rng = StdRng::seed_from_u64(seed);
        let mut params = vec![0.0; entries * (1 + hidden) + 2 * hidden];
        for p in &mut params[entries..entries * (1 + hidden)] {
            *p = rng.gen_range(-0.1..0.1);
        }
        Network { entries, hidden, params }
    }

    /// The network of `entries` entries and `hidden` units with `params`,
    /// such as one written out before.
    pub(crate) fn with_params(entries: usize, hidden: usize, params: Vec<f64>) -> Network {
        assert_eq!(params.len(), entries * (1 + hidden) + 2 * hidden, "the size of the network");
        Network { entries, hidden, params }
    }

    /// Where the units' biases start in `params`.
    fn biases(&self) -> usize {
        self.entries * (1 + self.hidden)
    }

    /// The units' sums before the larger of 0 is taken, for `entries`.
    fn sums(&self, entries: &[usize]) -> Vec<f64> {
        let h = self.hidden;
        let mut sums = self.params[self.biases()..self.biases() + h].to_vec();
        for &e in entries {
            let row = self.entries + e * h;
            for (sum, w) in sums.iter_mut().zip(&self.params[row..row + h]) {
                *sum += w;
            }
        }
        sums
    }

    /// The score of a candidate with `entries`.
    pub(crate) fn score(&self, entries: &[usize]) -> f64 {
        let linear: f64 = entries.iter().map(|&e| self.params[e]).sum();
        let outputs = &self.params[self.biases() + self.hidden..];
        let sums = self.sums(entries);
        linear + sums.iter().zip(outputs).map(|(s, o)| o * s.max(0.0)).sum::<f64>()
    }

    /// Adds `coefficient` times the gradient of the score of a candidate with
    /// `entries` to `gradient`.
    pub(crate) fn add_gradient(&self, entries: &[usize], coefficient: f64, gradient: &mut [f64]) {
        let (h, biases) = (self.hidden, self.biases());
        for &e in entries {
            gradient[e] += coefficient;
        }
        let sums = self.sums(entries);
        for (unit, &sum) in sums.iter().enumerate() {
            let output = self.params[biases + h + unit];
            gradient[biases + h + unit] += coefficient * sum.max(0.0);
            if sum > 0.0 {
                let through = coefficient * output;
                gradient[biases + unit] += through;
                for &e in entries {
                    gradient[self.entries + e * h + unit] += through;
                }
            }
        }
    }

    /// Which of `candidates` it scores highest.
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

    /// The mean value of the candidate it chooses, over `choices`.
    pub(crate) fn mean_value(&self, choices: &[&Vec<(Vec<usize>, f64)>]) -> f64 {
        let total: f64 = choices.iter().map(|c| c[self.choice(c)].1).sum();
        total / choices.len() as f64
    }

    /// The parameters, one a line, to be read back with [`Network::read`].
    pub(crate) fn write(&self, path: &str) {
        let text: String = self.params.iter().map(|w| format!("{w}\n")).collect();
        std::fs::write(path, text).unwrap();
    }

    /// The network of `entries` entries and `hidden` units written to the
    /// file `path`.
    pub(crate) fn read(path: &str, entries: usize, hidden: usize) -> Network {
        let text = std::fs::read_to_string(path).unwrap();
        Network::with_params(entries, hidden, text.lines().map(|l| l.parse().unwrap()).collect())
    }
}

/// Steps of Adam on a network's parameters, ascending.
pub(crate) struct Adam {
    first: Vec<f64>,
    second: Vec<f64>,
    steps: usize,
}

impl Adam {
    pub(crate) fn new(size: usize) -> Adam {
        Adam { first: vec![0.0; size], second: vec![0.0; size], steps: 0 }
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
    let mut network = Network::new(10, 3, 1);
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
