//! A play model file: one version of the play model, all in one file, as it
//! is kept as an asset of the GitHub release `play-models` (it is not kept in
//! git).  It holds the weights that the game plays by first, so that
//! `build.rs` can take them alone, and then all that self-play needs to go
//! on from it: the round, each learner of the population with its state of
//! Adam, and the league, the past models that take seats beside and against
//! the learners.  The game's model is the best of the learners when the file
//! was written, by their tournament.
//!
//! The layout, all little-endian: the 8 bytes `PLAYMDL2`; then, as `u32`s,
//! the round, the number of learners and the number of past models; then the
//! game's model; then for each learner, its model and Adam's steps as a `u32`
//! (0 for no state of Adam), and with a state of Adam, the first moments and
//! the second ones, as many `f32`s as it has weights; and for each past
//! model, its round as a `u32` and its model.  Each model is the length of
//! the name of its shape (see [`Shape`]) as a `u32`, that name, the number of
//! its weights as a `u32`, and its weights as `f32`s, so that the game's
//! model can be taken by reading from the 20th byte.
//!
//! A file of the first version, `PLAYMDL1`, has models of one shape, `v1`:
//! after the magic, the number of weights `n`, the number of entries, the
//! sizes of the first and second hidden layers, the round, the number of
//! learners and the number of past models; then the game's `n` weights; then
//! each learner's steps, `n` weights and moments; then each past model's
//! round and `n` weights.

use super::{Model, Shape, V1};
use crate::ai::fitting::Adam;

/// The bytes that a play model file starts with.
pub(crate) const MAGIC: &[u8; 8] = b"PLAYMDL2";

/// The bytes that a play model file of the first version starts with.
const MAGIC_1: &[u8; 8] = b"PLAYMDL1";

/// A learner of the population: its model, and its state of Adam if it has
/// one.
pub(crate) struct Learner {
    pub(crate) model: Model,
    pub(crate) adam: Option<Adam>,
}

/// One version of the play model, as a play model file holds it.
pub(crate) struct ModelFile {
    pub(crate) round: usize,
    /// The model that the game plays by.
    pub(crate) best: Model,
    pub(crate) learners: Vec<Learner>,
    /// The past models of the league, each with the round it is from.
    pub(crate) league: Vec<(usize, Model)>,
}

impl ModelFile {
    /// Writes it to `path`.
    pub(crate) fn write(&self, path: &str) {
        let mut bytes = MAGIC.to_vec();
        let number = |bytes: &mut Vec<u8>, number: usize| bytes.extend((number as u32).to_le_bytes());
        fn floats(bytes: &mut Vec<u8>, values: &[f64]) {
            bytes.extend(values.iter().flat_map(|&v| (v as f32).to_le_bytes()));
        }
        let model = |bytes: &mut Vec<u8>, model: &Model| {
            number(bytes, model.shape.name.len());
            bytes.extend(model.shape.name.as_bytes());
            number(bytes, model.network.params.len());
            floats(bytes, &model.network.params);
        };
        for n in [self.round, self.learners.len(), self.league.len()] {
            number(&mut bytes, n);
        }
        model(&mut bytes, &self.best);
        for learner in &self.learners {
            model(&mut bytes, &learner.model);
            number(&mut bytes, learner.adam.as_ref().map_or(0, |adam| adam.parts().2));
            if let Some(adam) = &learner.adam {
                let (first, second, _) = adam.parts();
                floats(&mut bytes, first);
                floats(&mut bytes, second);
            }
        }
        for (round, past) in &self.league {
            number(&mut bytes, *round);
            model(&mut bytes, past);
        }
        std::fs::write(path, bytes).unwrap();
    }

    /// Reads the file at `path`, of either version.
    pub(crate) fn read(path: &str) -> ModelFile {
        let bytes = std::fs::read(path).unwrap();
        let mut reader = Reader { bytes: &bytes, at: 8 };
        if &bytes[..8] == MAGIC_1 {
            return reader.first_version(path);
        }
        assert_eq!(&bytes[..8], MAGIC, "{path} is not a play model file");
        let [round, learners, league] = [0; 3].map(|_| reader.number());
        let best = reader.model(path);
        let learners = (0..learners)
            .map(|_| {
                let model = reader.model(path);
                let steps = reader.number();
                let n = model.network.params.len();
                let adam = (steps > 0).then(|| {
                    let first = reader.floats(n);
                    Adam::from_parts(first, reader.floats(n), steps)
                });
                Learner { model, adam }
            })
            .collect();
        let league = (0..league)
            .map(|_| {
                let past = reader.number();
                (past, reader.model(path))
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
    /// A model: the name of its shape, and its weights.
    fn model(&mut self, path: &str) -> Model {
        let length = self.number();
        let name = std::str::from_utf8(&self.bytes[self.at..self.at + length]).unwrap();
        self.at += length;
        let shape = Shape::named(name).unwrap_or_else(|| panic!("{path} has a model of the unknown shape {name}"));
        let n = self.number();
        assert_eq!(n, shape.size(), "{path} has a model of {name} with the wrong number of weights");
        Model::with_params(shape, self.floats(n))
    }

    /// The rest of a file of the first version, whose models are all `v1`.
    fn first_version(&mut self, path: &str) -> ModelFile {
        let [n, entries, hidden, second, round, learners, league] = [0; 7].map(|_| self.number());
        assert_eq!(
            (n, entries, hidden, second),
            (V1.size(), V1.entries(), V1.layers[0], V1.layers[1]),
            "{path} is a play model of another shape than v1"
        );
        let best = Model::with_params(&V1, self.floats(n));
        let learners = (0..learners)
            .map(|_| {
                let steps = self.number();
                let model = Model::with_params(&V1, self.floats(n));
                let adam = (steps > 0).then(|| {
                    let first = self.floats(n);
                    Adam::from_parts(first, self.floats(n), steps)
                });
                Learner { model, adam }
            })
            .collect();
        let league = (0..league)
            .map(|_| {
                let past = self.number();
                (past, Model::with_params(&V1, self.floats(n)))
            })
            .collect();
        ModelFile { round, best, learners, league }
    }

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai::fitted_play::V2_WIDE;

    /// A file holds models of several shapes, and reads back as it was
    /// written, to the precision of an `f32`.
    #[test]
    fn a_file_of_several_shapes_reads_back() {
        let model = |shape: &'static Shape, seed| Model::new(shape, seed);
        let mut adam = Adam::new(V2_WIDE.size());
        let mut wide = model(&V2_WIDE, 2);
        adam.step(&mut wide.network.params, &vec![0.5; V2_WIDE.size()], 0.01);
        let file = ModelFile {
            round: 7,
            best: model(&V1, 1),
            learners: vec![
                Learner { model: model(&V1, 1), adam: None },
                Learner { model: wide, adam: Some(adam) },
            ],
            league: vec![(5, model(&V2_WIDE, 3)), (6, model(&V1, 4))],
        };
        let path = std::env::temp_dir().join(format!("play-model-{}.bin", std::process::id()));
        let path = path.to_str().unwrap();
        file.write(path);
        let read = ModelFile::read(path);
        std::fs::remove_file(path).unwrap();
        let same = |a: &Model, b: &Model| {
            assert_eq!(a.shape, b.shape);
            for (x, y) in a.network.params.iter().zip(&b.network.params) {
                assert_eq!(*x as f32, *y as f32);
            }
        };
        assert_eq!(read.round, 7);
        same(&read.best, &file.best);
        for (a, b) in read.learners.iter().zip(&file.learners) {
            same(&a.model, &b.model);
            assert_eq!(a.adam.is_some(), b.adam.is_some());
        }
        assert_eq!(read.learners[1].adam.as_ref().unwrap().parts().2, 1);
        for (a, b) in read.league.iter().zip(&file.league) {
            assert_eq!(a.0, b.0);
            same(&a.1, &b.1);
        }
    }
}
