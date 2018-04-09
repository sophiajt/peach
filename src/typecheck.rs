use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Ty {
    U64,
    U32,
    Bool,
    Error,
    Void,
    Unknown,
    UnknownInt,
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Ty::U64 => "u64",
                Ty::U32 => "u32",
                Ty::Bool => "bool",
                Ty::Error => "{error}",
                Ty::Void => "void",
                Ty::Unknown => "{unknown}",
                Ty::UnknownInt => "{integer}",
            }
        )
    }
}

pub(crate) fn operator_compatible(lhs: &Ty, rhs: &Ty) -> bool {
    match (lhs, rhs) {
        (Ty::U64, Ty::U64)
        | (Ty::U32, Ty::U32)
        | (Ty::U64, Ty::UnknownInt)
        | (Ty::U32, Ty::UnknownInt)
        | (Ty::UnknownInt, Ty::U64)
        | (Ty::UnknownInt, Ty::U32)
        | (Ty::UnknownInt, Ty::UnknownInt) => true,
        _ => false,
    }
}

pub(crate) fn assignment_compatible(lhs: &Ty, rhs: &Ty) -> bool {
    if lhs == rhs {
        return true;
    }
    match (lhs, rhs) {
        (Ty::U64, Ty::UnknownInt)
        | (Ty::Unknown, _)
        | (Ty::U32, Ty::UnknownInt)
        | (Ty::UnknownInt, Ty::U64)
        | (Ty::UnknownInt, Ty::U32) => true,
        _ => false,
    }
}

pub(crate) fn tighter_of_types(lhs: &Ty, rhs: &Ty) -> Ty {
    match (lhs, rhs) {
        (Ty::U64, _) => Ty::U64,
        (Ty::U32, _) => Ty::U32,
        (Ty::Bool, _) => Ty::Bool,
        (_, Ty::U64) => Ty::U64,
        (_, Ty::U32) => Ty::U32,
        (_, Ty::Bool) => Ty::Bool,
        (Ty::Unknown, rhs) => rhs.clone(),
        (lhs, Ty::Unknown) => lhs.clone(),
        _ => lhs.clone(),
    }
}
