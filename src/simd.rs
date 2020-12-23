pub use crate::Card;
pub use crate::Suit;

/// A deck or hand of cards
///
/// This data structure assumes that each card can appear only once, as in the
/// game bridge.  If you want to allow more than one deck mixed together you
/// want a different crate.  Because each card can only appear once, `Cards` is
/// `Copy` and only takes 64 bits.  This makes it fast and memory efficient.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Cards {
    bits: packed_simd::u16x4,
}