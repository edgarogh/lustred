//! Collection of redstone gates with their properties

#[derive(Debug)]
pub struct Gate {
    pub name: &'static str,
    pub delay: usize,
    pub length: usize,

    /// Width in terms of lanes: actual block width is `width*2-1`
    pub width: usize,
}

pub const NOT: &'static Gate = &Gate {
    name: "NOT",
    delay: 1,
    length: 2,
    width: 1,
};

pub const AND: &'static Gate = &Gate {
    name: "AND",
    delay: 2,
    length: 2,
    width: 2,
};

pub const NAND: &'static Gate = &Gate {
    name: "NAND",
    delay: 1,
    length: 2,
    width: 2,
};

pub const OR: &'static Gate = &Gate {
    name: "OR",
    delay: 0,
    length: 1,
    width: 2,
};

pub const XOR: &'static Gate = &Gate {
    name: "XOR",
    delay: 2,
    length: 4,
    width: 2,
};
