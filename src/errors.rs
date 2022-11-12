use crate::sources::CodeArea;

#[derive(Debug)]
pub struct ErrorReport {
    pub title: String,
    pub message: String,
    pub labels: Vec<(CodeArea, String)>,
}

pub trait ToReport {
    fn to_report(&self) -> ErrorReport;
}

#[macro_export]
macro_rules! error_maker {
    (
        Title: $title:literal;
        pub enum $enum:ident {
            $(
                #[
                    Message: $msg:expr,
                    Labels: [
                        $(
                            $l_area:expr => $fmt:literal $(: $($e:expr),+)?;
                        )+
                    ]
                ]
                $err_name:ident {
                    $(
                        $field:ident: $typ:ty,
                    )*
                }
            )*
        }
    ) => {
        #[derive(Debug)]
        pub enum $enum {
            $(
                $err_name {
                    $(
                        $field: $typ,
                    )*
                },
            )*
        }
        use $crate::errors::{ToReport, ErrorReport};
        impl ToReport for $enum {
            fn to_report(&self) -> ErrorReport {
                match self {
                    $(
                        $enum::$err_name { $($field,)* } => ErrorReport {
                            title: $title.to_string(),
                            message: ($msg).to_string(),
                            labels: vec![
                                $(
                                    ($l_area.clone(), format!($fmt $(, $($e),*)?)),
                                )*
                            ],
                        },
                    )*
                }
            }
        }
    };
}
