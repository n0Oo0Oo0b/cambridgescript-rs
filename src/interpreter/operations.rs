use std::{
    ops::{Add, Div, Mul, Neg, Not, Sub},
    rc::Rc,
};

use crate::tree_parser::{Pow, Value};

macro_rules! impl_arithmetic {
    ( $trait:path; $fn:ident [$op:tt] $( { $( $other:tt )* } )? ) => {
        impl $trait for Value {
            type Output = Option<Value>;

            fn $fn(self, rhs: Self) -> Self::Output {
                Some(match (self, rhs) {
                    (Value::Integer(a), Value::Integer(b)) => Value::Integer(a $op b),
                    (Value::Real(a), Value::Real(b)) => Value::Real(a $op b),
                    $($( $other )*)?
                    _ => return None,
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
    type Output = Option<Value>;

    fn pow(self, rhs: Self) -> Self::Output {
        Some(match (self, rhs) {
            (Value::Integer(b), Value::Integer(e)) => Value::Integer(b.pow(e as u32)),
            (Value::Real(b), Value::Real(e)) => Value::Real(b.powf(e)),
            _ => return None,
        })
    }
}

impl TryFrom<Value> for bool {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Boolean(b) => Ok(b),
            _ => Err(()),
        }
    }
}

impl Neg for Value {
    type Output = Option<Value>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Integer(x) => Some(Value::Integer(-x)),
            Value::Real(x) => Some(Value::Real(-x)),
            // TODO: incompatible type for bool
            _ => None,
        }
    }
}

impl Not for Value {
    type Output = Option<Value>;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(b) => Some((!b).into()),
            // TODO: incompatible type for bool
            _ => None,
        }
    }
}
