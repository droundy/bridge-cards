//! The fit that the robot's n-tuple networks share, for the tests that fit
//! them.  A network scores each candidate of a choice, such as each card that
//! could be played, by adding up the table entries that the candidate looks
//! up.  Fitting makes the candidates of most value the likeliest, where a
//! candidate is chosen at random with odds that grow exponentially with its
//! score, by gradient ascent on the mean value of that choice.  Each entry is
//! a feature that is 1 when a candidate looks it up, so the score is linear
//! in the weights.

/// For each choice, each candidate's entries and its value to the one who
/// chooses, the more the better.
pub(crate) type Choices = Vec<Vec<(Vec<usize>, f64)>>;

/// The score of a candidate with `entries` by `weights`.
pub(crate) fn score(weights: &[f64], entries: &[usize]) -> f64 {
    entries.iter().map(|&e| weights[e]).sum()
}

/// Which of `candidates` `weights` scores highest.
pub(crate) fn choice(weights: &[f64], candidates: &[(Vec<usize>, f64)]) -> usize {
    let mut best = 0;
    for (i, (entries, _)) in candidates.iter().enumerate() {
        if score(weights, entries) > score(weights, &candidates[best].0) {
            best = i;
        }
    }
    best
}

/// The mean value of the candidate that `weights` chooses, over `choices`.
pub(crate) fn mean_value(weights: &[f64], choices: &[&Vec<(Vec<usize>, f64)>]) -> f64 {
    let total: f64 = choices.iter().map(|c| c[choice(weights, c)].1).sum();
    total / choices.len() as f64
}

/// Weights for `size` entries fitted to `choices` by `steps` steps of Adam,
/// with a step that shrinks to nothing, less `penalty` times the squares of
/// the weights.
pub(crate) fn fit(choices: &[&Vec<(Vec<usize>, f64)>], size: usize, penalty: f64, steps: usize) -> Vec<f64> {
    let mut weights = vec![0.0; size];
    let (mut first, mut second) = (vec![0.0; size], vec![0.0; size]);
    for step in 1..=steps {
        let mut gradient: Vec<f64> = weights.iter().map(|w| -penalty * w).collect();
        for candidates in choices {
            let scores: Vec<f64> = candidates.iter().map(|(e, _)| score(&weights, e)).collect();
            let most = scores.iter().copied().fold(f64::MIN, f64::max);
            let odds: Vec<f64> = scores.iter().map(|s| (s - most).exp()).collect();
            let sum: f64 = odds.iter().sum();
            let mean: f64 = odds.iter().zip(*candidates).map(|(p, c)| p * c.1).sum::<f64>() / sum;
            for ((entries, value), p) in candidates.iter().zip(&odds) {
                let g = p / sum * (value - mean) / choices.len() as f64;
                for &e in entries {
                    gradient[e] += g;
                }
            }
        }
        let t = step as f64;
        let rate = 0.05 * (1.0 - (t - 1.0) / steps as f64);
        for j in 0..size {
            first[j] = 0.9 * first[j] + 0.1 * gradient[j];
            second[j] = 0.999 * second[j] + 0.001 * gradient[j] * gradient[j];
            let a = first[j] / (1.0 - 0.9f64.powf(t));
            let b = second[j] / (1.0 - 0.999f64.powf(t));
            weights[j] += rate * a / (b.sqrt() + 1e-8);
        }
    }
    weights
}

/// The penalty of `penalties` whose fits choose best on held-out choices, by
/// five-fold cross-validation, the folds running at once, with the mean
/// value held out that each gives.  The choice `i` is in fold `i % 5`.
pub(crate) fn cross_validate(
    choices: &Choices,
    size: usize,
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
                        let weights = fit(&split(false), size, penalty, steps);
                        mean_value(&weights, &held) * held.len() as f64
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
