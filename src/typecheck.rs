pub(crate) type TypeId = usize;

pub mod builtin_type {
    use super::*;
    pub const UNKNOWN: TypeId = 0;
    pub const UNKNOWN_INT: TypeId = 1;
    pub const VOID: TypeId = 2;
    pub const U64: TypeId = 3;
    pub const U32: TypeId = 4;
    pub const I64: TypeId = 5;
    pub const I32: TypeId = 6;
    pub const BOOL: TypeId = 7;
    pub const ERROR: TypeId = 8;
}

pub(crate) struct StructType {
    pub(crate) fields: Vec<(String, TypeId)>,
}

pub(crate) enum TypeInfo {
    Builtin,
    Struct(StructType),
}

pub struct TypeChecker {
    pub(crate) types: Vec<TypeInfo>,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        let mut types = vec![];

        for _ in 0..(builtin_type::ERROR + 1) {
            types.push(TypeInfo::Builtin);
        }

        TypeChecker { types }
    }

    pub fn new_struct(&mut self, fields: Vec<(String, TypeId)>) -> TypeId {
        self.types.push(TypeInfo::Struct(StructType { fields }));

        self.types.len() - 1
    }

    pub fn is_custom_type(&self, type_id: TypeId) -> bool {
        type_id > builtin_type::ERROR
    }

    pub fn printable_name(&self, type_id: TypeId) -> String {
        match type_id {
            builtin_type::UNKNOWN => "{unknown}".into(),
            builtin_type::UNKNOWN_INT => "{unknown int}".into(),
            builtin_type::VOID => "void".into(),
            builtin_type::U64 => "u64".into(),
            builtin_type::U32 => "u32".into(),
            builtin_type::I64 => "i64".into(),
            builtin_type::I32 => "i32".into(),
            builtin_type::BOOL => "bool".into(),
            builtin_type::ERROR => "{error}".into(),
            _ => format!("{{custom type: {}}}", type_id),
        }
    }

    pub(crate) fn operator_compatible(&self, lhs: TypeId, rhs: TypeId) -> bool {
        if lhs == rhs {
            return true;
        }
        match (lhs, rhs) {
            | (builtin_type::U64, builtin_type::UNKNOWN_INT)
            | (builtin_type::U32, builtin_type::UNKNOWN_INT)
            | (builtin_type::I64, builtin_type::UNKNOWN_INT)
            | (builtin_type::I32, builtin_type::UNKNOWN_INT)
            | (builtin_type::UNKNOWN_INT, builtin_type::U64)
            | (builtin_type::UNKNOWN_INT, builtin_type::U32)
            | (builtin_type::UNKNOWN_INT, builtin_type::I64)
            | (builtin_type::UNKNOWN_INT, builtin_type::I32)
            | (builtin_type::UNKNOWN_INT, builtin_type::UNKNOWN_INT) => true,
            _ => false,
        }
    }

    pub(crate) fn assignment_compatible(&self, lhs: TypeId, rhs: TypeId) -> bool {
        if lhs == rhs {
            return true;
        }
        match (lhs, rhs) {
            (builtin_type::U64, builtin_type::UNKNOWN_INT)
            | (builtin_type::U32, builtin_type::UNKNOWN_INT)
            | (builtin_type::I64, builtin_type::UNKNOWN_INT)
            | (builtin_type::I32, builtin_type::UNKNOWN_INT)
            | (builtin_type::UNKNOWN, _)
            | (builtin_type::UNKNOWN_INT, builtin_type::U64)
            | (builtin_type::UNKNOWN_INT, builtin_type::U32)
            | (builtin_type::UNKNOWN_INT, builtin_type::I64)
            | (builtin_type::UNKNOWN_INT, builtin_type::I32) => true,
            _ => false,
        }
    }

    pub(crate) fn tighter_of_types(&self, lhs: TypeId, rhs: TypeId) -> TypeId {
        match (lhs, rhs) {
            (builtin_type::U64, _) => builtin_type::U64,
            (builtin_type::U32, _) => builtin_type::U32,
            (builtin_type::I64, _) => builtin_type::I64,
            (builtin_type::I32, _) => builtin_type::I32,
            (builtin_type::BOOL, _) => builtin_type::BOOL,
            (_, builtin_type::U64) => builtin_type::U64,
            (_, builtin_type::U32) => builtin_type::U32,
            (_, builtin_type::I64) => builtin_type::I64,
            (_, builtin_type::I32) => builtin_type::I32,
            (_, builtin_type::BOOL) => builtin_type::BOOL,
            (builtin_type::UNKNOWN, rhs) => rhs.clone(),
            (lhs, builtin_type::UNKNOWN) => lhs.clone(),
            _ => lhs.clone(),
        }
    }
}
