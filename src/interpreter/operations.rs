use std::{
    ops::{Add, Div, Mul, Neg, Not, Sub},
    rc::Rc,
};

use crate::tree_parser::{Pow, Value};

use super::runtime::{RuntimeError, RuntimeResult};

macro_rules! impl_arithmetic {
    ( $trait:path; $fn:ident [$op:tt] $( { $( $other:tt )* } )? ) => {
        impl $trait for Value {
            type Output = RuntimeResult<Value>;

            fn $fn(self, rhs: Self) -> Self::Output {
                Ok(match (self, rhs) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a $op b),
                    (Value::Real(a), Value::Real(b)) => Value::Real(a $op b),
                    $($( $other )*)?
                    (l, r) => return Err(RuntimeError::IncompatibleTypes(l, r)),
                })
            }
        }
    };
}

impl_arithmetic!(Add; add [+] {
    (Value::String(a), Value::String(b)) => {
        let result = a.to_string() + b.as_ref();
        Value::String(Rc::from(result.into_boxed_str()))
    }
});
impl_arithmetic!(Sub; sub [-]);
impl_arithmetic!(Mul; mul [*]);
impl_arithmetic!(Div; div [/]);

impl Pow for Value {
    type Output = RuntimeResult<Value>;

    fn pow(self, rhs: Self) -> RuntimeResult<Value> {
        Ok(match (self, rhs) {
            (Value::Integer(b), Value::Integer(e)) => Value::Integer(b.pow(e as u32)),
            (Value::Real(b), Value::Real(e)) => Value::Real(b.powf(e)),
            (l, r) => return Err(RuntimeError::IncompatibleTypes(l, r)),
        })
    }
}

impl Neg for Value {
    type Output = RuntimeResult<Value>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Integer(x) => Ok(Value::Integer(-x)),
            Value::Real(x) => Ok(Value::Real(-x)),
            // TODO: incompatible type for bool
            x => Err(RuntimeError::IncompatibleTypes(x.clone(), x)),
        }
    }
}

impl Not for Value {
    type Output = RuntimeResult<Value>;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(b) => Ok((!b).into()),
            // TODO: incompatible type for bool
            x => Err(RuntimeError::IncompatibleTypes(x.clone(), x)),
        }
    }
}
