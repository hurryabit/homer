/*
Tests for type equivalence.
*/

use super::*;
use crate::checker::types::EquivChecker;
use crate::checker::types::Type;

#[test]
fn all() {
    let module = check_output(
        r#"
        type Unit = {};
        type Pair<X, Y> = {fst: X, snd: Y};
        type HomPair<X> = {fst: X, snd: X};
        type Option<X> = [None | Some(X)];
        type Sum<X, Y> = [Left(X) | Right(Y)];
        type HomSum<X> = [Left(X) | Right(X)];
        type List<X> = [Nil | Cons({head: X, tail: List<X>})];
        type Id<X> = X;
        type Const<X, Y> = X;

        type T_X<X> = {
            a: X,
            b: Id<X>,
        };

        type T_Y<Y> = {
            a: Y,
            b: Id<Y>,
        };

        type SynBool = Bool;
        type T_Bool = {
            a: Bool,
            b: SynBool,
        };

        type SynInt = Int;
        type T_Int = {
            a: Int,
            b: SynInt,
            c: Id<Int>,
            d: Const<Int, Int>,
            e: Const<Int, Bool>,
        };

        type T_Unit = {
            a: Unit,
            b: {},
        };

        type T_PairIntBool = {
            a: Pair<Int, Bool>,
            b: {fst: Int, snd: Bool},
            c: Pair<SynInt, SynBool>,
            d: {fst: SynInt, snd: SynBool},
        };
        type T_PairBoolBool = {
            a: Pair<Bool, Bool>,
            b: HomPair<Bool>,
        };
        type T_PairIntInt = {
            a: Pair<Int, Int>,
            b: HomPair<Int>,
        };
        type T_PairBoolInt = { a: Pair<Bool, Int> };
        type T_PairXBool<X> = { a: Pair<X, Bool> };
        type T_PairIntY<Y> = { a: Pair<Int, Y> };
        type T_PairXY<X, Y> = { a: Pair<X, Y> };
        type T_PairYX<X, Y> = { a: Pair<Y, X> };
        type T_RevPairIntBool = { a: {snd: Bool, fst: Int} };

        type T_OptionInt = {
            a: Option<Int>,
            b: [None | Some(Int)],
            c: Option<SynInt>,
            d: [None | Some(SynInt)],
        };
        type T_OptionBool = { a: Option<Bool> };
        type T_OptionX<X> = { a: Option<X> };
        type T_OptionY<Y> = { a: Option<Y> };

        type T_SumIntBool = {
            a: Sum<Int, Bool>,
            b: [Left(Int) | Right(Bool)],
            c: Sum<SynInt, SynBool>,
            d: [Left(SynInt) | Right(SynBool)],
        };
        type T_SumBoolBool = {
            a: Sum<Bool, Bool>,
            b: HomSum<Bool>,
        };
        type T_SumIntInt = {
            a: Sum<Int, Int>,
            b: HomSum<Int>,
        };
        type T_SumBoolInt = { a: Sum<Bool, Int> };
        type T_RevSumIntBool = { a: [Right(Bool) | Left(Int)] };

        type T_FunIntBoolToUnit = {
            a: (Int, Bool) -> Unit,
            b: (SynInt, SynBool) -> {},
        };
        type T_FunBoolBoolToUnit = { a: (Bool, Bool) -> Unit };
        type T_FunIntIntToUnit = { a: (Int, Int) -> Unit };
        type T_FunIntBoolToInt = { a: (Int, Bool) -> Int };
        type T_FunXYToZ<X, Y, Z> = { a: (X, Y) -> Z };
        type T_FunYXToZ<X, Y, Z> = { a: (Y, X) -> Z };
        type T_FunIntToUnit = { a: (Int) -> Unit };
        type T_FunBoolToUnit = { a: (Bool) -> Unit };
        type T_FunXToY<X, Y> = { a: (X) -> Y };
        type T_FunYToX<X, Y> = { a: (Y) -> X };
        type T_FunToInt = { a: () -> Int };
        type T_FunToBool = { a: () -> Bool };
        type T_FunToX<X> = { a: () -> X };

        type IntList = [Nil | Cons({head: Int, tail: IntList})];
        type List1<X> = [Nil | Cons({head: X, tail: List2<X>})];
        type List2<X> = [Nil | Cons({head: X, tail: List1<X>})];
        type EvenList<X> = [Nil | Cons({head: X, tail: OddList<X>})];
        type OddList<X> = [Cons({head: X, tail: EvenList<X>})];
        type AltList<X, Y> =
            [ Nil
            | Cons({head: X, tail: [Nil | Cons({head: Y, tail: AltList<X, Y>})]})
            ];
        type AltList1<X, Y> = [Nil | Cons({head: X, tail: AltList2<Y, X>})];
        type AltList2<X, Y> = [Nil | Cons({head: X, tail: AltList1<Y, X>})];
        type AltList3<X, Y> = [Nil | Cons({head: X, tail: AltList4<X, Y>})];
        type AltList4<X, Y> = [Nil | Cons({head: Y, tail: AltList3<X, Y>})];
        type EvenAltList<X, Y> = [Nil | Cons({head: X, tail: OddAltList<Y, X> })];
        type OddAltList<X, Y> = [Cons({head: X, tail: EvenAltList<Y, X>})];
        type T_ListInt = {
            a: List<Int>,
            b: IntList,
            c: List<SynInt>,
            d: List1<Int>,
            e: List2<Int>,
            f: AltList<Int, Int>,
        };
        type T_ListBool = {
            a: List<Bool>,
            b: AltList<Bool, Bool>,
        };
        type T_ListX<X> = { a: List<X> };
        type T_ListY<Y> = { a: List<Y> };
        type T_ListListInt = { a: List<List<Int>> };
        type T_EvenListInt = {
            a: EvenList<Int>,
            b: EvenAltList<Int, Int>,
        };
        type T_OddListInt = {
            a: OddList<Int>,
            b: OddAltList<Int, Int>,
        };
        type T_AltListIntBool = {
            a: AltList<Int, Bool>,
            b: AltList1<Int, Bool>,
            c: AltList2<Int, Bool>,
            d: AltList3<Int, Bool>,
            e: AltList4<Bool, Int>,
        };
        type T_AltListBoolInt = { a: AltList<Bool, Int> };
        type T_EvenAltListIntBool = { a: EvenAltList<Int, Bool> };
        type T_EvenAltListBoolInt = { a: EvenAltList<Bool, Int> };
        type T_OddAltListIntBool = { a: OddAltList<Int, Bool> };
        type T_OddAltListBoolInt = { a: OddAltList<Bool, Int> };


        type Tree<X> = [Leaf | Branch({value: X, children: List<Tree<X>>})];
        type IntTree = [Leaf | Branch({value: Int, children: IntTreeList})];
        type IntTreeList = [Nil | Cons({head: IntTree, tail: IntTreeList})];
        type IntTree2 = [Leaf | Branch({value: Int, children: List<IntTree2>})];
        type T_TreeInt = {
            a: Tree<Int>,
            b: IntTree,
            c: IntTree2,
            d: Tree<SynInt>,
        };
        type T_TreeBool = { a: Tree<Bool> };
        type T_TreeTreeInt = { a: Tree<Tree<Int>> };
        type T_ListTreeInt = { a: List<Tree<Int>> };
        type T_TreeListInt = { a: Tree<List<Int>> };

        type Phantom1<X> = [Phantom];
        type Phantom2<X, Y> = [Phantom];
        type T_Phantom = {
            a: Phantom1<Int>,
            b: Phantom1<Bool>,
            c: Phantom2<Int, Bool>,
            d: Phantom2<Bool, Bool>,
            e: Phantom2<Int, Int>,
            f: Phantom2<Bool, Int>,
            g: [Phantom],
        };

        type Syn = [Syn];
        type SynSyn = Syn;
        type SynId<X> = Id<X>;
        type SynSynId<X> = SynId<X>;
        type T_Syn = {
            a: [Syn],
            b: Syn,
            c: SynSyn,
            d: Id<[Syn]>,
            e: Id<Syn>,
            f: Id<SynSyn>,
            g: SynId<[Syn]>,
            h: SynId<Syn>,
            i: SynId<SynSyn>,
            j: SynSynId<[Syn]>,
            k: SynSynId<Syn>,
            l: SynSynId<SynSyn>,
            m: Id<Id<[Syn]>>,
            n: SynId<SynId<Syn>>,
            o: SynId<SynId<SynSyn>>,
            p: Const<Syn, Syn>,
            q: Const<Syn, Int>,
        };
        "#,
    );

    let type_defs = module.type_defs();
    let cases = type_defs
        .iter()
        .filter_map(|(name, schema)| {
            if name.as_str().starts_with("T_") {
                let Type::Record(fields) = &*schema.body else {
                    panic!("tets case {name} is not a record");
                };
                Some((name, fields))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    for (case1, subcases1) in &cases {
        for (case2, subcases2) in &cases {
            let expected = case1 == case2;
            for (subcase1, typ1) in *subcases1 {
                for (subcase2, typ2) in *subcases2 {
                    let actual = EquivChecker::new(&type_defs).check(typ1, typ2);
                    assert_eq!(
                        actual,
                        expected,
                        "expected {case1}.{subcase1}={typ1} and {case2}.{subcase2}={typ2} to be {} but they are not",
                        if expected { "equivalent" } else { "not equivalent" },
                    )
                }
            }
        }
    }
}
