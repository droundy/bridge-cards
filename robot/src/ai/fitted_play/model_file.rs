//! A play model file: one version of the play model, all in one file, as it
//! is kept as an asset of the GitHub release `play-models` (it is not kept in
//! git).  It holds the weights that the game plays by first, so that
//! `build.rs` can take them alone, and then all that self-play needs to go
//! on from it: the round, each learner of the population with its state of
//! Adam, and the league, the past models that take seats beside and against
//! the learners.  The game's model is the best of the learners when the file
//! was written, by their tournament.
//!
//! The layout, all little-endian: the 8 bytes `PLAYMDL1`; then, as `u32`s,
//! the number of weights `n`, the number of entries, the sizes of the first
//! and second hidden layers, the round, the number of learners, and the
//! number of past models; then the game's `n` `f32` weights; then for each
//! learner, Adam's steps as a `u32` (0 for no state of Adam), its `n` `f32`
//! weights, and with a state of Adam, `n` `f32` first moments and `n` second
//! ones; and for each past model, its round as a `u32` and its `n` `f32`
//! weights.

use crate::ai::fitting::{Adam, Network};

/// The bytes that a play model file starts with.
pub(crate) const MAGIC: &[u8; 8] = b"PLAYMDL1";

/// A learner of the population: its weights, and its state of Adam if it
/// has one.
pub(crate) struct Learner {
    pub(crate) weights: Network,
    pub(crate) adam: Option<Adam>,
}

/// One version of the play model, as a play model file holds it.
pub(crate) struct ModelFile {
    pub(crate) round: usize,
    /// The model that the game plays by.
    pub(crate) best: Network,
    pub(crate) learners: Vec<Learner>,
    /// The past models of the league, each with the round it is from.
    pub(crate) league: Vec<(usize, Network)>,
}

impl ModelFile {
    /// Writes it to `path`.
    pub(crate) fn write(&self, path: &str) {
        let (entries, hidden, second) = self.best.shape();
        let mut bytes = MAGIC.to_vec();
        let header = [
            self.best.params.len(),
            entries,
            hidden,
            second,
            self.round,
            self.learners.len(),
            self.league.len(),
        ];
        for number in header {
            bytes.extend((number as u32).to_le_bytes());
        }
        fn floats(bytes: &mut Vec<u8>, values: &[f64]) {
            bytes.extend(values.iter().flat_map(|&v| (v as f32).to_le_bytes()));
        }
        floats(&mut bytes, &self.best.params);
        for learner in &self.learners {
            let steps = learner.adam.as_ref().map_or(0, |adam| adam.parts().2);
            bytes.extend((steps as u32).to_le_bytes());
            floats(&mut bytes, &learner.weights.params);
            if let Some(adam) = &learner.adam {
                let (first, second, _) = adam.parts();
                floats(&mut bytes, first);
                floats(&mut bytes, second);
            }
        }
        for (round, network) in &self.league {
            bytes.extend((*round as u32).to_le_bytes());
            floats(&mut bytes, &network.params);
        }
        std::fs::write(path, bytes).unwrap();
    }

    /// Reads the file at `path`, which must be of a network of the shape
    /// `entries`, `hidden`, `second`.
    pub(crate) fn read(path: &str, entries: usize, hidden: usize, second: usize) -> ModelFile {
        let bytes = std::fs::read(path).unwrap();
        assert_eq!(&bytes[..8], MAGIC, "{path} is not a play model file");
        let mut reader = Reader { bytes: &bytes, at: 8 };
        let [n, file_entries, file_hidden, file_second, round, learners, league] = [0; 7].map(|_| reader.number());
        assert_eq!(
            (file_entries, file_hidden, file_second),
            (entries, hidden, second),
            "{path} is a play model of another shape"
        );
        let network = |reader: &mut Reader| Network::with_params(entries, hidden, second, reader.floats(n));
        let best = network(&mut reader);
        let learners = (0..learners)
            .map(|_| {
                let steps = reader.number();
                let weights = network(&mut reader);
                let adam = (steps > 0).then(|| {
                    let first = reader.floats(n);
                    Adam::from_parts(first, reader.floats(n), steps)
                });
                Learner { weights, adam }
            })
            .collect();
        let league = (0..league)
            .map(|_| {
                let past = reader.number();
                (past, network(&mut reader))
            })
            .collect();
        ModelFile { round, best, learners, league }
    }
}

/// Reads a play model file from the start onwards.
struct Reader<'a> {
    bytes: &'a [u8],
    at: usize,
}

impl Reader<'_> {
    fn number(&mut self) -> usize {
        let n = u32::from_le_bytes(self.bytes[self.at..self.at + 4].try_into().unwrap());
        self.at += 4;
        n as usize
    }

    fn floats(&mut self, n: usize) -> Vec<f64> {
        let values = self.bytes[self.at..self.at + 4 * n]
            .as_chunks::<4>()
            .0
            .iter()
            .map(|b| f32::from_le_bytes(*b) as f64)
            .collect();
        self.at += 4 * n;
        values
    }
}
