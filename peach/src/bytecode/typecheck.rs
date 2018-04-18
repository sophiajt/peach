use bytecode::engine::{BytecodeEngine, DefinitionId};

pub mod builtin_type {
    use super::*;
    pub const UNKNOWN: DefinitionId = 0;
    pub const UNKNOWN_INT: DefinitionId = 1;
    pub const VOID: DefinitionId = 2;
    pub const U64: DefinitionId = 3;
    pub const U32: DefinitionId = 4;
    pub const I64: DefinitionId = 5;
    pub const I32: DefinitionId = 6;
    pub const BOOL: DefinitionId = 7;
    pub const VOID_PTR: DefinitionId = 8;
    pub const ERROR: DefinitionId = 9;
}

impl BytecodeEngine {
    pub fn is_custom_type(&self, type_id: DefinitionId) -> bool {
        type_id > builtin_type::ERROR
    }

    pub fn printable_name(&self, type_id: DefinitionId) -> String {
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

    pub(crate) fn operator_compatible(&self, lhs: DefinitionId, rhs: DefinitionId) -> bool {
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

    pub(crate) fn assignment_compatible(&self, lhs: DefinitionId, rhs: DefinitionId) -> bool {
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

    pub(crate) fn tighter_of_types(&self, lhs: DefinitionId, rhs: DefinitionId) -> DefinitionId {
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
