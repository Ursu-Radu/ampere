use crate::sources::CodeArea;

#[derive(Debug)]
pub struct ErrorReport {
    pub title: String,
    pub message: String,
    pub labels: Vec<(CodeArea, String)>,
}

#[macro_export]
macro_rules! error_maker {
    (
        Title: $title:literal
        Extra: {
            $(
                $extra_arg:ident: $extra_type:ty,
            )*
        }
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
        use $crate::errors::ErrorReport;
        impl $enum {
            pub fn to_report(&self $(, $extra_arg: $extra_type)* ) -> ErrorReport {
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
