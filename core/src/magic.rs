use crate::{
    moves,
    square::{BySquare, File, Rank, Square},
    squareset::SquareSet,
};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct MagicEntry {
    pub mask: SquareSet,
    pub magic: u64,
    pub offset: usize,
}

impl MagicEntry {
    #[inline]
    pub fn index(&self, occupied: SquareSet, bits: u32) -> usize {
        let relevant = occupied & self.mask;
        let index = relevant.0.wrapping_mul(self.magic) >> (64 - bits);
        index as usize + self.offset
    }
}

#[inline]
pub fn rook_table_index(square: Square, occupied: SquareSet) -> usize {
    ROOK_MAGICS[square].index(occupied, ROOK_MAGIC_BITS)
}

#[inline]
pub fn bishop_table_index(square: Square, occupied: SquareSet) -> usize {
    BISHOP_MAGICS[square].index(occupied, BISHOP_MAGIC_BITS)
}

#[inline]
pub fn bishop_relevant_blockers(square: Square) -> SquareSet {
    moves::bishop(square, SquareSet::EMPTY) & !SquareSet::EDGES
}

#[inline]
pub fn rook_relevant_blockers(square: Square) -> SquareSet {
    use {File::*, Rank::*};
    let file = SquareSet::from_file(square.file());
    let rank = SquareSet::from_rank(square.rank());

    ((file & !(SquareSet::from_rank(First) | SquareSet::from_rank(Eighth)))
        | (rank & !(SquareSet::from_file(A) | SquareSet::from_file(H))))
        & !SquareSet::from_square(square)
}

pub const SLIDING_TABLE_SIZE: usize = 120175;
pub const ROOK_MAGIC_BITS: u32 = 12;
pub const BISHOP_MAGIC_BITS: u32 = 9;

#[rustfmt::skip]
const ROOK_MAGICS: BySquare<MagicEntry> = BySquare::from_array([
    MagicEntry { magic: 0x008004a012c00081, mask: SquareSet(0x000101010101017e), offset: 5783 },
    MagicEntry { magic: 0x0020000800100020, mask: SquareSet(0x000202020202027c), offset: 9879 },
    MagicEntry { magic: 0x0040100008004005, mask: SquareSet(0x000404040404047a), offset: 11927 },
    MagicEntry { magic: 0x0040080040040002, mask: SquareSet(0x0008080808080876), offset: 13975 },
    MagicEntry { magic: 0x0040040002004001, mask: SquareSet(0x001010101010106e), offset: 16023 },
    MagicEntry { magic: 0x0020200080010214, mask: SquareSet(0x002020202020205e), offset: 18071 },
    MagicEntry { magic: 0x0040008001000040, mask: SquareSet(0x004040404040403e), offset: 20163 },
    MagicEntry { magic: 0x0080004028800300, mask: SquareSet(0x008080808080807e), offset: 22211 },
    MagicEntry { magic: 0x0080440400220100, mask: SquareSet(0x0001010101017e00), offset: 26307 },
    MagicEntry { magic: 0x0000100008040013, mask: SquareSet(0x0002020202027c00), offset: 28558 },
    MagicEntry { magic: 0x0000080403020008, mask: SquareSet(0x0004040404047a00), offset: 29582 },
    MagicEntry { magic: 0x0000200400020020, mask: SquareSet(0x0008080808087600), offset: 30736 },
    MagicEntry { magic: 0x0080200200200100, mask: SquareSet(0x0010101010106e00), offset: 31760 },
    MagicEntry { magic: 0x0000202001000080, mask: SquareSet(0x0020202020205e00), offset: 32784 },
    MagicEntry { magic: 0x0000200040008020, mask: SquareSet(0x0040404040403e00), offset: 33808 },
    MagicEntry { magic: 0x0080200040880020, mask: SquareSet(0x0080808080807e00), offset: 34832 },
    MagicEntry { magic: 0x0040002000100022, mask: SquareSet(0x00010101017e0100), offset: 36948 },
    MagicEntry { magic: 0x0004001000080010, mask: SquareSet(0x00020202027c0200), offset: 38996 },
    MagicEntry { magic: 0x0002000804010008, mask: SquareSet(0x00040404047a0400), offset: 40020 },
    MagicEntry { magic: 0x0002002004202006, mask: SquareSet(0x0008080808760800), offset: 41045 },
    MagicEntry { magic: 0x0002002020010002, mask: SquareSet(0x00101010106e1000), offset: 42087 },
    MagicEntry { magic: 0x0101001000804010, mask: SquareSet(0x00202020205e2000), offset: 43111 },
    MagicEntry { magic: 0x0100801020004010, mask: SquareSet(0x00404040403e4000), offset: 44135 },
    MagicEntry { magic: 0x0000802000400020, mask: SquareSet(0x00808080807e8000), offset: 45159 },
    MagicEntry { magic: 0x0220490010080010, mask: SquareSet(0x000101017e010100), offset: 47208 },
    MagicEntry { magic: 0x0000080010040010, mask: SquareSet(0x000202027c020200), offset: 49405 },
    MagicEntry { magic: 0x0004010008020008, mask: SquareSet(0x000404047a040400), offset: 50429 },
    MagicEntry { magic: 0x0000040020200200, mask: SquareSet(0x0008080876080800), offset: 51453 },
    MagicEntry { magic: 0x0312008010010010, mask: SquareSet(0x001010106e101000), offset: 52477 },
    MagicEntry { magic: 0x0080010020008020, mask: SquareSet(0x002020205e202000), offset: 53501 },
    MagicEntry { magic: 0x0000400040008001, mask: SquareSet(0x004040403e404000), offset: 54525 },
    MagicEntry { magic: 0x0004200020004081, mask: SquareSet(0x008080807e808000), offset: 55549 },
    MagicEntry { magic: 0x0040001000200020, mask: SquareSet(0x0001017e01010100), offset: 57609 },
    MagicEntry { magic: 0x0010020400080008, mask: SquareSet(0x0002027c02020200), offset: 59657 },
    MagicEntry { magic: 0x0004010200080008, mask: SquareSet(0x0004047a04040400), offset: 60681 },
    MagicEntry { magic: 0x0100200400200200, mask: SquareSet(0x0008087608080800), offset: 61705 },
    MagicEntry { magic: 0x0000200100200200, mask: SquareSet(0x0010106e10101000), offset: 62729 },
    MagicEntry { magic: 0x0000200080200100, mask: SquareSet(0x0020205e20202000), offset: 63753 },
    MagicEntry { magic: 0x0000008000404001, mask: SquareSet(0x0040403e40404000), offset: 64777 },
    MagicEntry { magic: 0x0000802000200040, mask: SquareSet(0x0080807e80808000), offset: 65801 },
    MagicEntry { magic: 0x0040201004000802, mask: SquareSet(0x00017e0101010100), offset: 67850 },
    MagicEntry { magic: 0x0010080102000400, mask: SquareSet(0x00027c0202020200), offset: 69901 },
    MagicEntry { magic: 0x0040044008004010, mask: SquareSet(0x00047a0404040400), offset: 70926 },
    MagicEntry { magic: 0x0000020004002020, mask: SquareSet(0x0008760808080800), offset: 72206 },
    MagicEntry { magic: 0x0000020001002020, mask: SquareSet(0x00106e1010101000), offset: 73230 },
    MagicEntry { magic: 0x0000010000802020, mask: SquareSet(0x00205e2020202000), offset: 74254 },
    MagicEntry { magic: 0x0000400080004001, mask: SquareSet(0x00403e4040404000), offset: 75278 },
    MagicEntry { magic: 0x0040020490008801, mask: SquareSet(0x00807e8080808000), offset: 76302 },
    MagicEntry { magic: 0x0040001000200020, mask: SquareSet(0x007e010101010100), offset: 78590 },
    MagicEntry { magic: 0x0200080010040010, mask: SquareSet(0x007c020202020200), offset: 80638 },
    MagicEntry { magic: 0x0044000802010008, mask: SquareSet(0x007a040404040400), offset: 81662 },
    MagicEntry { magic: 0x0200200400020020, mask: SquareSet(0x0076080808080800), offset: 82686 },
    MagicEntry { magic: 0x0000200200010020, mask: SquareSet(0x006e101010101000), offset: 83710 },
    MagicEntry { magic: 0x0008010020008020, mask: SquareSet(0x005e202020202000), offset: 84734 },
    MagicEntry { magic: 0x0008200040008020, mask: SquareSet(0x003e404040404000), offset: 85758 },
    MagicEntry { magic: 0x001c802000400020, mask: SquareSet(0x007e808080808000), offset: 86782 },
    MagicEntry { magic: 0x0000c10910208001, mask: SquareSet(0x7e01010101010100), offset: 88831 },
    MagicEntry { magic: 0x0000088100201041, mask: SquareSet(0x7c02020202020200), offset: 92927 },
    MagicEntry { magic: 0x0000080440801022, mask: SquareSet(0x7a04040404040400), offset: 96767 },
    MagicEntry { magic: 0x0000401004200802, mask: SquareSet(0x7608080808080800), offset: 100735 },
    MagicEntry { magic: 0x0300041002080001, mask: SquareSet(0x6e10101010101000), offset: 104575 },
    MagicEntry { magic: 0x0000048004280201, mask: SquareSet(0x5e20202020202000), offset: 108415 },
    MagicEntry { magic: 0x0140020400428001, mask: SquareSet(0x3e40404040404000), offset: 112247 },
    MagicEntry { magic: 0x0201802100834402, mask: SquareSet(0x7e80808080808000), offset: 116079 },
]);

#[rustfmt::skip]
const BISHOP_MAGICS: BySquare<MagicEntry> = BySquare::from_array([
    MagicEntry { magic: 0x0002004200801002, mask: SquareSet(0x0040201008040200), offset: 0 },
    MagicEntry { magic: 0x0000210040080801, mask: SquareSet(0x0000402010080400), offset: 64 },
    MagicEntry { magic: 0x0000800810200014, mask: SquareSet(0x0000004020100a00), offset: 96 },
    MagicEntry { magic: 0x0000806004040480, mask: SquareSet(0x0000000040221400), offset: 128 },
    MagicEntry { magic: 0x0200440200003200, mask: SquareSet(0x0000000002442800), offset: 176 },
    MagicEntry { magic: 0x001021c100901108, mask: SquareSet(0x0000000204085000), offset: 239 },
    MagicEntry { magic: 0x0200804100804800, mask: SquareSet(0x0000020408102000), offset: 299 },
    MagicEntry { magic: 0x0020082202020040, mask: SquareSet(0x0002040810204000), offset: 361 },
    MagicEntry { magic: 0x0008004200404008, mask: SquareSet(0x0020100804020000), offset: 487 },
    MagicEntry { magic: 0x0080010020204002, mask: SquareSet(0x0040201008040000), offset: 519 },
    MagicEntry { magic: 0x0200010040080200, mask: SquareSet(0x00004020100a0000), offset: 551 },
    MagicEntry { magic: 0x01840080481002c5, mask: SquareSet(0x0000004022140000), offset: 583 },
    MagicEntry { magic: 0x0080004402022102, mask: SquareSet(0x0000000244280000), offset: 631 },
    MagicEntry { magic: 0x02249021c1008020, mask: SquareSet(0x0000020408500000), offset: 694 },
    MagicEntry { magic: 0x0000184042008042, mask: SquareSet(0x0002040810200000), offset: 754 },
    MagicEntry { magic: 0x0004002011008020, mask: SquareSet(0x0004081020400000), offset: 816 },
    MagicEntry { magic: 0x0004300041002020, mask: SquareSet(0x0010080402000200), offset: 878 },
    MagicEntry { magic: 0x0000208200204011, mask: SquareSet(0x0020100804000400), offset: 910 },
    MagicEntry { magic: 0x0102000100080881, mask: SquareSet(0x004020100a000a00), offset: 942 },
    MagicEntry { magic: 0x0080220200801020, mask: SquareSet(0x0000402214001400), offset: 1070 },
    MagicEntry { magic: 0x0040240080841000, mask: SquareSet(0x0000024428002800), offset: 1198 },
    MagicEntry { magic: 0x005010008044004d, mask: SquareSet(0x0002040850005000), offset: 1336 },
    MagicEntry { magic: 0x0000080010410022, mask: SquareSet(0x0004081020002000), offset: 1464 },
    MagicEntry { magic: 0x0000041010108018, mask: SquareSet(0x0008102040004000), offset: 1496 },
    MagicEntry { magic: 0x0002010080810014, mask: SquareSet(0x0008040200020400), offset: 1528 },
    MagicEntry { magic: 0x0000404440208082, mask: SquareSet(0x0010080400040800), offset: 1560 },
    MagicEntry { magic: 0x00c1010000820005, mask: SquareSet(0x0020100a000a1000), offset: 1592 },
    MagicEntry { magic: 0x0002080144024008, mask: SquareSet(0x0040221400142200), offset: 1720 },
    MagicEntry { magic: 0x0001001001014001, mask: SquareSet(0x0002442800284400), offset: 2232 },
    MagicEntry { magic: 0x0080204000804032, mask: SquareSet(0x0004085000500800), offset: 2744 },
    MagicEntry { magic: 0x0020400800108010, mask: SquareSet(0x0008102000201000), offset: 2872 },
    MagicEntry { magic: 0x0008080200104020, mask: SquareSet(0x0010204000402000), offset: 2904 },
    MagicEntry { magic: 0x0100420061008102, mask: SquareSet(0x0004020002040800), offset: 2936 },
    MagicEntry { magic: 0x0020208200402020, mask: SquareSet(0x0008040004081000), offset: 2968 },
    MagicEntry { magic: 0x0201008040820012, mask: SquareSet(0x00100a000a102000), offset: 3000 },
    MagicEntry { magic: 0x0000040109040100, mask: SquareSet(0x0022140014224000), offset: 3128 },
    MagicEntry { magic: 0x0010440400404100, mask: SquareSet(0x0044280028440200), offset: 3640 },
    MagicEntry { magic: 0x0000802081004012, mask: SquareSet(0x0008500050080400), offset: 4152 },
    MagicEntry { magic: 0x000010400a201080, mask: SquareSet(0x0010200020100800), offset: 4280 },
    MagicEntry { magic: 0x0000800404800820, mask: SquareSet(0x0020400040201000), offset: 4312 },
    MagicEntry { magic: 0x0000404200800045, mask: SquareSet(0x0002000204081000), offset: 4344 },
    MagicEntry { magic: 0x0008404040450020, mask: SquareSet(0x0004000408102000), offset: 4376 },
    MagicEntry { magic: 0x0028808080800042, mask: SquareSet(0x000a000a10204000), offset: 4408 },
    MagicEntry { magic: 0x0102408840400020, mask: SquareSet(0x0014001422400000), offset: 4536 },
    MagicEntry { magic: 0x0000020040102102, mask: SquareSet(0x0028002844020000), offset: 4669 },
    MagicEntry { magic: 0x0000480020205040, mask: SquareSet(0x0050005008040200), offset: 4797 },
    MagicEntry { magic: 0x0002008020051808, mask: SquareSet(0x0020002010080400), offset: 4925 },
    MagicEntry { magic: 0x0240204010200002, mask: SquareSet(0x0040004020100800), offset: 4957 },
    MagicEntry { magic: 0x0000802082005062, mask: SquareSet(0x0000020408102000), offset: 4989 },
    MagicEntry { magic: 0x0300208040402000, mask: SquareSet(0x0000040810204000), offset: 5051 },
    MagicEntry { magic: 0x0004040820806180, mask: SquareSet(0x00000a1020400000), offset: 5113 },
    MagicEntry { magic: 0x0020800008403000, mask: SquareSet(0x0000142240000000), offset: 5149 },
    MagicEntry { magic: 0x0000000100202014, mask: SquareSet(0x0000284402000000), offset: 5210 },
    MagicEntry { magic: 0x00c0010200101086, mask: SquareSet(0x0000500804020000), offset: 5242 },
    MagicEntry { magic: 0x0204010020200602, mask: SquareSet(0x0000201008040200), offset: 5274 },
    MagicEntry { magic: 0x0000204080200402, mask: SquareSet(0x0000402010080400), offset: 5306 },
    MagicEntry { magic: 0x0008202100480048, mask: SquareSet(0x0002040810204000), offset: 5338 },
    MagicEntry { magic: 0x0000082080404020, mask: SquareSet(0x0004081020400000), offset: 5464 },
    MagicEntry { magic: 0x0240000408208060, mask: SquareSet(0x000a102040000000), offset: 5526 },
    MagicEntry { magic: 0x0300081200084030, mask: SquareSet(0x0014224000000000), offset: 5562 },
    MagicEntry { magic: 0x005a000001002020, mask: SquareSet(0x0028440200000000), offset: 5623 },
    MagicEntry { magic: 0x0000010200410008, mask: SquareSet(0x0050080402000000), offset: 5655 },
    MagicEntry { magic: 0x0010010100108008, mask: SquareSet(0x0020100804020000), offset: 5687 },
    MagicEntry { magic: 0x0080420020401040, mask: SquareSet(0x0040201008040200), offset: 5719 },
]);
