use swc_plugin::{ast::*, plugin_transform, syntax_pos::DUMMY_SP, TransformPluginProgramMetadata};

pub struct TransformVisitor;

fn is_react_createelement(expr: &CallExpr) -> bool {
    match &expr.callee {
        Callee::Expr(expr) => match expr.as_ref() {
            Expr::Member(MemberExpr { obj, prop, .. }) => match (obj.as_ref(), prop) {
                (Expr::Ident(obj), MemberProp::Ident(prop)) => {
                    obj.sym.to_string() == "React" && prop.sym.to_string() == "createElement"
                }
                _ => false,
            },
            _ => false,
        },
        _ => false,
    }
}

fn get_jsx_node(call_expr: &CallExpr) -> Option<Expr> {
    if !is_react_createelement(&call_expr) {
        return None;
    }

    let mut args = call_expr.args.iter();
    let name_node = args.next();
    let props_node = args.next();
    let child_nodes = args.collect::<Vec<_>>();

    let name = get_jsx_name(match name_node {
        Some(ExprOrSpread { spread: None, expr }) => Some(expr.as_ref()),
        _ => None,
    }?)?;
    let props = get_jsx_props(props_node);
    let children = child_nodes
        .into_iter()
        .filter_map(|child| match child {
            ExprOrSpread { spread: None, expr } => Some(get_jsx_child(expr.as_ref())),
            _ => None,
        })
        .flatten()
        .collect::<Vec<_>>();
    let children_is_empty = children.is_empty();

    Some(
        Box::new(JSXElement {
            span: DUMMY_SP,
            opening: JSXOpeningElement {
                name: name.clone(),
                span: DUMMY_SP,
                attrs: props,
                self_closing: children_is_empty,
                type_args: None,
            },
            children,
            closing: match children_is_empty {
                true => None,
                false => Some(JSXClosingElement {
                    name,
                    span: DUMMY_SP,
                }),
            },
        })
        .into(),
    )
}

fn get_jsx_name(name_node: &Expr) -> Option<JSXElementName> {
    match name_node {
        Expr::Ident(ident) => Some(ident.clone().into()),
        Expr::Lit(Lit::Str(lit)) => Some(Ident::new(lit.value.clone(), DUMMY_SP).into()),
        Expr::Member(MemberExpr {
            obj,
            prop: MemberProp::Ident(prop),
            ..
        }) => get_jsx_name(&obj)
            .and_then(|par| match par {
                JSXElementName::Ident(ident) => Some(ident.into()),
                JSXElementName::JSXMemberExpr(member) => Some(Box::new(member).into()),
                JSXElementName::JSXNamespacedName(_) => None,
            })
            .map(|obj| {
                JSXMemberExpr {
                    obj,
                    prop: prop.clone(),
                }
                .into()
            }),
        _ => None,
    }
}

fn get_jsx_props(props_node: Option<&ExprOrSpread>) -> Vec<JSXAttrOrSpread> {
    match props_node {
        Some(ExprOrSpread { spread: None, expr }) => {
            if let Expr::Call(CallExpr {
                callee: Callee::Expr(expr),
                args,
                ..
            }) = expr.as_ref()
            {
                if let Expr::Ident(callee_ident) = expr.as_ref() {
                    if callee_ident.sym.to_string() == "_extends" {
                        return args
                            .iter()
                            .map(Some)
                            .flat_map(get_jsx_props)
                            .collect::<Vec<_>>();
                    }
                }
            }

            match expr.as_ref() {
                Expr::Object(obj) => obj
                    .props
                    .iter()
                    .filter_map(|prop| match prop {
                        PropOrSpread::Prop(prop) => {
                            let kv = match prop.as_ref() {
                                Prop::KeyValue(KeyValueProp {
                                    key: PropName::Ident(ident),
                                    value,
                                    ..
                                }) => Some((ident.clone(), value.clone())),
                                Prop::KeyValue(KeyValueProp {
                                    key: PropName::Str(key),
                                    value,
                                    ..
                                }) => {
                                    Some((Ident::new(key.value.clone(), DUMMY_SP), value.clone()))
                                }
                                Prop::Shorthand(ident) => {
                                    Some((ident.clone(), Box::new(Expr::Ident(ident.clone()))))
                                }
                                Prop::Method(MethodProp {
                                    key: PropName::Ident(ident),
                                    function,
                                }) => {
                                    Some((
                                        ident.clone(),
                                        Box::<Expr>::new(
                                            FnExpr {
                                                ident: Some(ident.clone()),
                                                function: function.clone(),
                                            }
                                            .into(),
                                        )
                                        .clone(),
                                    ))
                                }
                                _ => None,
                            };
                            kv.map(|(key, value)| {
                                JSXAttr {
                                    span: DUMMY_SP,
                                    name: key.into(),
                                    value: Some(match *value {
                                        Expr::Lit(Lit::Str(str)) => Lit::Str(str).into(),
                                        _ => JSXExprContainer {
                                            span: DUMMY_SP,
                                            expr: value.into(),
                                        }
                                        .into(),
                                    }),
                                }
                                .into()
                            })
                        }
                        PropOrSpread::Spread(spread) => Some(spread.clone().into()),
                    })
                    .collect(),
                Expr::Lit(Lit::Null(_)) => vec![],
                e => vec![SpreadElement {
                    dot3_token: DUMMY_SP,
                    expr: Box::new(e.clone()),
                }
                .into()],
            }
        }
        _ => Vec::new(),
    }
}

fn get_jsx_child(child: &Expr) -> Vec<JSXElementChild> {
    match child {
        Expr::Lit(Lit::Str(str)) => {
            return vec![JSXText {
                span: DUMMY_SP,
                value: str.value.clone(),
                raw: str.raw.clone().unwrap_or(str.value.clone()),
            }
            .into()];
        }
        Expr::Call(call_expr) => {
            if is_react_createelement(call_expr) {
                match get_jsx_node(call_expr) {
                    Some(Expr::JSXElement(el)) => return vec![el.into()],
                    Some(Expr::JSXFragment(fragment)) => return vec![fragment.into()],
                    _ => (),
                };
            }
        }
        Expr::Array(expr) => {
            return expr
                .elems
                .iter()
                .filter_map(|expr| match expr {
                    Some(ExprOrSpread {
                        spread: None, expr, ..
                    }) => Some(get_jsx_child(expr.as_ref())),
                    _ => None,
                })
                .flatten()
                .collect::<Vec<_>>();
        }
        _ => (),
    }

    vec![JSXElementChild::JSXExprContainer(JSXExprContainer {
        span: DUMMY_SP,
        expr: Box::new(child.clone()).into(),
    })]
}

impl VisitMut for TransformVisitor {
    // Implement necessary visit_mut_* methods for actual custom transform.
    // A comprehensive list of possible visitor methods can be found here:
    // https://rustdoc.swc.rs/swc_ecma_visit/trait.VisitMut.html

    fn visit_mut_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Call(call_expr) => {
                if let Some(new_expr) = get_jsx_node(call_expr) {
                    *expr = new_expr;
                }
            }
            _ => (),
        }
    }
}

/// An example plugin function with macro support.
/// `plugin_transform` macro interop pointers into deserialized structs, as well
/// as returning ptr back to host.
///
/// It is possible to opt out from macro by writing transform fn manually via
/// `__plugin_process_impl(
///     ast_ptr: *const u8,
///     ast_ptr_len: i32,
///     config_str_ptr: *const u8,
///     config_str_ptr_len: i32,
///     context_str_ptr: *const u8,
///     context_str_ptr_len: i32) ->
///     i32 /*  0 for success, fail otherwise.
///             Note this is only for internal pointer interop result,
///             not actual transform result */
///
/// if plugin need to handle low-level ptr directly. However, there are
/// important steps manually need to be performed like sending transformed
/// results back to host. Refer swc_plugin_macro how does it work internally.
#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    program.fold_with(&mut as_folder(TransformVisitor))
}

#[cfg(test)]
mod tests {
    use swc_ecma_parser::{EsConfig, Syntax};
    use swc_ecma_transforms_base::resolver;
    use swc_ecma_transforms_testing::test;
    use swc_plugin::{
        ast::{as_folder, Fold},
        chain,
        syntax_pos::Mark,
    };

    use crate::TransformVisitor;

    fn tr() -> impl Fold {
        chain! {
            resolver(Mark::new(), Mark::new(), false),
            as_folder(TransformVisitor)
        }
    }

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        blank,
        "function App() {
            return React.createElement('div');
        }",
        "function App() {
            return <div />;
        }"
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        with_str_lit_attrs,
        r#"
        function App() {
            return React.createElement('div', { foo: 'bar' });
        }
        "#,
        r#"
        function App() {
            return <div foo='bar' />;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        with_non_str_lit_attrs,
        r#"
        function App() {
            return React.createElement('div', { foo: 0, bar: true });
        }
        "#,
        r#"
        function App() {
            return <div foo={0} bar={true} />;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        shorthand_obj,
        r#"
        function App() {
            const foo = 'bar'
            return React.createElement('div', { foo });
        }
        "#,
        r#"
        function App() {
            const foo = 'bar'
            return <div foo={foo} />;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        spread_props,
        r#"
        function App() {
            const props = { foo: 'bar' }
            return React.createElement('div', props);
        }
        "#,
        r#"
        function App() {
            const props = { foo: 'bar' }
            return <div {...props} />;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        props_mixed,
        r#"
        function App() {
            const a = 'b'
            const props = { c: 'd' }
            return React.createElement('div', {
                a,
                ...props,
                e: 'f',
                g: 0,
                h: true
            });
        }
        "#,
        r#"
        function App() {
            const a = 'b'
            const props = { c: 'd' }
            return <div a={a} {...props} e='f' g={0} h={true} />;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        with_str_child,
        r#"
        function App() {
            return React.createElement('div', null, 'Hello, world!');
        }
        "#,
        r#"
        function App() {
            return <div>Hello, world!</div>;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        with_expr_child,
        r#"
        function App() {
            return React.createElement('div', null, 42);
        }
        "#,
        r#"
        function App() {
            return <div>{42}</div>;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        with_element_child,
        r#"
        function App() {
            return React.createElement(
                'div',
                null,
                React.createElement('div')
            );
        }
        "#,
        r#"
        function App() {
            return <div><div /></div>;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        with_children_mixed,
        r#"
        function App() {
            return React.createElement(
                'div',
                null,
                'Hello, world!',
                42,
                React.createElement('div'),
            );
        }
        "#,
        r#"
        function App() {
            return <div>Hello, world!{42}<div /></div>;
        }
        "#
    );

    test!(
        Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        |_| tr(),
        full,
        r#"
        function App() {
            const props = { qux: 'quux' }

            return React.createElement(
                MyComponent,
                {
                    foo: 'bar',
                    baz: 42,
                    ...props,
                },
                'Hello, world!',
                42,
                React.createElement(
                    'div',
                    { foo: 'bar' },
                ),
                React.createElement(
                    AnotherComponent,
                    { fn () { return null } },
                    'duh',
                )
            );
        }
        "#,
        r#"
        function App() {
            const props = { qux: 'quux' }

            return <MyComponent foo='bar' baz={42} {...props}>Hello, world!{42}<div foo='bar' /><AnotherComponent fn={function fn() { return null }}>duh</AnotherComponent></MyComponent>;
        }
        "#
    );
}
