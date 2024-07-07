/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

package pisc
package parser

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.util.parsing.combinator._

import Pi.Names
import Expression.ExpressionParsingException


class Expression extends JavaTokenParsers:

  import scala.meta._
  import scala.meta.dialects.Scala3

  /** Scala comment enclosing any [[Scalameta]] [[Term]]
    * or [[Enumerator]]s (used for assignment)
    * @return
    */
  def expression: Parser[(Either[List[Enumerator], Term], Names)] =
    """[/][*].*?[*][/]""".r ^^ { it =>
      val expr = it.stripPrefix("/*").stripSuffix("*/")
      try
        Expression(expr.parse[Term].get) match
          case (term, names) => Right(term) -> names
      catch _ =>
        try
          Expression(("for {" + expr + " } yield ()").parse[Term].get) match
            case (Term.ForYield(enums, _), names) =>
              Left(enums) -> names
        catch t =>
          throw ExpressionParsingException(expr, t)
    }


object Expression:

  class ParsingException(msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause)

  case class ExpressionParsingException(expr: String, cause: Throwable)
      extends ParsingException(s"Expression /*$expr*/ is not a valid Scalameta Term or Enumerator", cause)


  import scala.Function.const

  import scala.{ meta => sm }


  inline def apply(self: sm.Term): (sm.Term, Names) =
    Term(self)


  object UnzipReduce:

    def apply[T <: sm.Tree | List[? <: sm.Tree]](r: List[(T, Names)]): (List[T], Names) =
      r.map(_._1) -> r.map(_._2).foldLeft(Names())(_ ++ _)


  // C ///////////////////////////////////////////////////////////////////////

  object Case:

    def apply(self: List[sm.Case]): (List[sm.Case], Names) =
      val (cs: List[sm.Case], csns) = UnzipReduce(self.map(CaseTree(_)))
      cs -> csns

  object CaseTree:

    def apply(self: sm.CaseTree): (sm.CaseTree, Names) = self match

      case it @ sm.Case(pat, cond, body) =>
        val (p, pns) = Pat(pat)
        val Some((c, cns)) = cond.map(Term(_)).orElse(Some(Term.`null -> Nil`))
        val (b, bns) = Term(body)
        it.copy(pat = p, cond = cond.map(const(c)), body = b) -> (pns ++ cns ++ bns)

      case it @ sm.TypeCase(pat, body) =>
        val (p, pns) = Type(pat)
        val (b, bns) = Type(body)
        it.copy(pat = p, body = b) -> (pns ++ bns)


  object Ctor:

    def apply(self: sm.Ctor): (sm.Ctor, Names) = self match

      case it @ sm.Ctor.Primary(mods, _, paramss) =>
        val (ms, msns) = Stat.Mod(mods)
        val (ps, psns) = Term.Param(paramss)
        it.copy(mods = ms, paramss = ps) -> (msns ++ psns)

      case it @ sm.Ctor.Secondary(mods, _, paramss, init, stats) =>
        val (ms, msns) = Stat.Mod(mods)
        val (ps, psns) = Term.Param(paramss)
        val (i, ins) = Init(init)
        val (ss, ssns) = Stat(stats)
        it.copy(mods = ms, paramss = ps, init = i, stats = ss) -> (msns ++ psns ++ ins ++ ssns)


  // E ///////////////////////////////////////////////////////////////////////

  object Enumerator:

    def apply(self: List[sm.Enumerator]): (List[sm.Enumerator], Names) =
      UnzipReduce(self.map(this(_)))

    def apply(self: sm.Enumerator): (sm.Enumerator, Names) = self match

      case it @ sm.Enumerator.Generator(pat, rhs) =>
        val (p, pns) = Pat(pat)
        val (r, rns) = Term(rhs)
        it.copy(pat = p, rhs = r) -> (pns ++ rns)

      case it @ sm.Enumerator.CaseGenerator(pat, rhs) =>
        val (p, pns) = Pat(pat)
        val (r, rns) = Term(rhs)
        it.copy(pat = p, rhs = r) -> (pns ++ rns)

      case it @ sm.Enumerator.Val(pat, rhs) =>
        val (p, pns) = Pat(pat)
        val (r, rns) = Term(rhs)
        it.copy(pat = p, rhs = r) -> (pns ++ rns)

      case it @ sm.Enumerator.Guard(cond) =>
        val (c, cns) = Term(cond)
        it.copy(cond = c) -> cns


  // I ///////////////////////////////////////////////////////////////////////

  object Init:

    def apply(self: List[sm.Init]): (List[sm.Init], Names) =
      UnzipReduce(self.map(this(_)))

    def apply(self: sm.Init): (sm.Init, Names) = self match

      case it @ sm.Init(tpe, _, argss) =>
        val (t, tns) = Type(tpe)
        val (as, asns) = UnzipReduce(argss.map(Term(_)))
        it.copy(tpe = t, argss = as) -> (tns ++ asns)


  // P ///////////////////////////////////////////////////////////////////////

  object Pat:

    def apply(self: List[sm.Pat]): (List[sm.Pat], Names) =
      UnzipReduce(self.map(this(_)))

    def apply(self: sm.Pat): (sm.Pat, Names) = self match

      case it @ sm.Pat.Alternative(lhs, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Pat.Bind(lhs, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Pat.Extract(fun, args) =>
        val (f, fns) = Term(fun)
        val (as, asns) = this(args)
        it.copy(fun = f, args = as) -> (fns ++ asns)

      case it @ sm.Pat.ExtractInfix(lhs, _, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Pat.Given(tpe) =>
        val (t, tns) = Type(tpe)
        it.copy(tpe = t) -> tns

      case it @ sm.Pat.Interpolate(_, _, args) =>
        val (as, asns) = this(args)
        it.copy(args = as) -> asns

      case sm.Lit.Symbol(free @ Symbol(name)) =>
        sm.Pat.Var(sm.Term.Name(name)) -> Set(free)

      case it @ sm.Pat.Macro(body) =>
        val (b, bns) = Term(body)
        it.copy(body = b) -> bns

      case it @ sm.Pat.Tuple(args) =>
        val (as, asns) = this(args)
        it.copy(args = as) -> asns

      case it @ sm.Pat.Typed(lhs, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = Type(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Pat.Xml(_, args) =>
        val (as, asns) = this(args)
        it.copy(args = as) -> asns

      case it @ sm.Term.Name(name) if name.startsWith("'") =>
        val free = Symbol(name.stripPrefix("'"))
        sm.Pat.Var(sm.Term.Name(free.name)) -> Set(free)

      case it @ sm.Term.Select(qual, _) =>
        val (q, qns) = Term(qual)
        it.copy(qual = q) -> qns

      case it =>
        it -> Names()


  // R ///////////////////////////////////////////////////////////////////////

  object Ref:

    def apply(self: sm.Ref): (sm.Ref, Names) = self match

      case it: sm.Init =>
        Init(it)

      case _: sm.Importee => ???

      case it =>
        it -> Names()


  // S ///////////////////////////////////////////////////////////////////////

  object Stat:

    def apply(self: List[sm.Stat]): (List[sm.Stat], Names) =
      UnzipReduce(self.map(this(_)))

    def apply(self: sm.Stat): (sm.Stat, Names) = self match

      case it: sm.Term =>
        Term(it)

      case it: sm.Decl =>
        Decl(it)

      case it: sm.Defn =>
        Defn(it)

      case it =>
        it -> Names()


    // D ///////////////////////////////////////////////////////////////////////

    object Decl:

      def apply(self: sm.Decl): (sm.Decl, Names) = self match

        case it @ sm.Decl.Def(mods, _, tparams, paramss, decltpe) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (ps, psns) = Term.Param(paramss)
          val (dt, dtns) = Type(decltpe)
          it.copy(mods = ms, tparams = ts, paramss = ps, decltpe = dt) -> (msns ++ tsns ++ psns ++ dtns)

        case it @ sm.Decl.Given(mods, _, tparams, sparams, decltpe) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (ps, psns) = Term.Param(sparams)
          val (dt, dtns) = Type(decltpe)
          it.copy(mods = ms, tparams = ts, sparams = ps, decltpe = dt) -> (msns ++ tsns ++ psns ++ dtns)

        case it @ sm.Decl.Type(mods, _, tparams, bounds) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (bs, bsns) = Type(bounds)
          it.copy(mods = ms, tparams = ts, bounds = bs) -> (msns ++ tsns ++ bsns)

        case it @ sm.Decl.Val(mods, pats, decltpe) =>
          val (ms, msns) = Mod(mods)
          val (ps, psns) = Pat(pats)
          val (dt, dtns) = Type(decltpe)
          it.copy(mods = ms, pats = ps, decltpe = dt) -> (msns ++ psns ++ dtns)

        case it @ sm.Decl.Var(mods, pats, decltpe) =>
          val (ms, msns) = Mod(mods)
          val (ps, psns) = Pat(pats)
          val (dt, dtns) = Type(decltpe)
          it.copy(mods = ms, pats = ps, decltpe = dt) -> (msns ++ psns ++ dtns)


    object Defn:

      def apply(self: sm.Defn): (sm.Defn, Names) = self match

        case it @ sm.Defn.Class(mods, _, tparams, ctor, templ) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (c: sm.Ctor.Primary, cns) = Ctor(ctor)
          val (t, tns) = Template(templ)
          it.copy(mods = ms, tparams = ts, ctor = c, templ = t) -> (msns ++ tsns ++ cns ++ tns)

        case it @ sm.Defn.Def(mods, _, tparams, paramss, decltpe, body) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (ps, psns) = Term.Param(paramss)
          val Some((dt, dtns)) = decltpe.map(Type(_)).orElse(Some(Type.`null -> Nil`))
          val (b, bns) = Term(body)
          it.copy(mods = ms, tparams = ts, paramss = ps, decltpe = decltpe.map(const(dt)), body = b) -> (msns ++ tsns ++ psns ++ dtns ++ bns)

        case it @ sm.Defn.Enum(mods, _, tparams, ctor, templ) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (c: sm.Ctor.Primary, cns) = Ctor(ctor)
          val (t, tns) = Template(templ)
          it.copy(mods = ms, tparams = ts, ctor = c, templ = t) -> (msns ++ tsns ++ cns ++ tns)

        case it @ sm.Defn.EnumCase(mods, _, tparams, ctor, inits) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (c: sm.Ctor.Primary, cns) = Ctor(ctor)
          val (is, isns) = Init(inits)
          it.copy(mods = ms, tparams = ts, ctor = c, inits = is) -> (msns ++ tsns ++ cns ++ isns)

        case it @ sm.Defn.ExtensionGroup(tparams, paramss, body) =>
          val (ts, tsns) = Type.Param(tparams)
          val (ps, psns) = Term.Param(paramss)
          val (b, bns) = Stat(body)
          it.copy(tparams = ts, paramss = ps, body = b) -> (tsns ++ psns ++ bns)

        case it @ sm.Defn.Given(mods, _, tparams, sparams, templ) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (ps, psns) = Term.Param(sparams)
          val (t, tns) = Template(templ)
          it.copy(mods = ms, tparams = ts, sparams = ps, templ = t) -> (msns ++ tsns ++ psns ++ tns)

        case it @ sm.Defn.GivenAlias(mods, _, tparams, sparams, decltpe, body) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (ps, psns) = Term.Param(sparams)
          val (dt, dtns) = Type(decltpe)
          val (b, bns) = Term(body)
          it.copy(mods = ms, tparams = ts, sparams = ps, decltpe = dt, body = b) -> (msns ++ tsns ++ psns ++ dtns ++ bns)

        case it @ sm.Defn.Macro(mods, _, tparams, paramss, decltpe, body) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (ps, psns) = Term.Param(paramss)
          val Some((dt, dtns)) = decltpe.map(Type(_)).orElse(Some(Type.`null -> Nil`))
          val (b, bns) = Term(body)
          it.copy(mods = ms, tparams = ts, paramss = ps, decltpe = decltpe.map(const(dt)), body = b) -> (msns ++ tsns ++ psns ++ dtns ++ bns)

        case it @ sm.Defn.Object(mods, _, templ) =>
          val (ms, msns) = Mod(mods)
          val (t, tns) = Template(templ)
          it.copy(mods = ms, templ = t) -> (msns ++ tns)

        case it @ sm.Defn.RepeatedEnumCase(mods, _) =>
          val (ms, msns) = Mod(mods)
          it.copy(mods = ms) -> msns

        case it @ sm.Defn.Trait(mods, _, tparams, ctor, templ) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (c: sm.Ctor.Primary, cns) = Ctor(ctor)
          val (t, tns) = Template(templ)
          it.copy(mods = ms, tparams = ts, ctor = c, templ = t) -> (msns ++ tsns ++ cns ++ tns)

        case it @ sm.Defn.Type(mods, _, tparams, body) =>
          val (ms, msns) = Mod(mods)
          val (ts, tsns) = Type.Param(tparams)
          val (b, bns) = Type(body)
          it.copy(mods = ms, tparams = ts, body = b) -> (msns ++ tsns ++ bns)

        case it @ sm.Defn.Val(mods, pats, decltpe, rhs) =>
          val (ms, msns) = Mod(mods)
          val (ps, psns) = Pat(pats)
          val Some((dt, dtns)) = decltpe.map(Type(_)).orElse(Some(Type.`null -> Nil`))
          val (r, rns) = Term(rhs)
          it.copy(mods = ms, pats = ps, decltpe = decltpe.map(const(dt)), rhs = r) -> (msns ++ psns ++ dtns ++ rns)

        case it @ sm.Defn.Var(mods, pats, decltpe, rhs) =>
          val (ms, msns) = Mod(mods)
          val (ps, psns) = Pat(pats)
          val Some((dt, dtns)) = decltpe.map(Type(_)).orElse(Some(Type.`null -> Nil`))
          val Some((r, rns)) = rhs.map(Term(_)).orElse(Some(Term.`null -> Nil`))
          it.copy(mods = ms, pats = ps, decltpe = decltpe.map(const(dt)), rhs = rhs.map(const(r))) -> (msns ++ psns ++ dtns ++ rns)


    // M ///////////////////////////////////////////////////////////////////////

    object Mod:

      def apply(self: List[sm.Mod]): (List[sm.Mod], Names) =
        UnzipReduce(self.map(this(_)))

      def apply(self: sm.Mod): (sm.Mod, Names) = self match

        case it @ sm.Mod.Annot(init) =>
          val (i, ins) = Init(init)
          it.copy(init = i) -> ins

        case it =>
          it -> Names()


  // T ///////////////////////////////////////////////////////////////////////

  object Template:

    def apply(self: sm.Template): (sm.Template, Names) = self match

      case it @ sm.Template(early, inits, self @ sm.Self(_, decltpe), stats) =>
        val (es, esns) = Stat(early)
        val (is, isns) = Init(inits)
        val Some((dt, dtns)) = decltpe.map(Type(_)).orElse(Some(Type.`null -> Nil`))
        val (ss, ssns) = Stat(stats)
        it.copy(early = es, inits = is, self = self.copy(decltpe = decltpe.map(const(dt))), stats = ss) -> (esns ++ isns ++ dtns ++ ssns)


  object Term:

    val `null -> Nil` = null.asInstanceOf[sm.Term] -> Names()

    def apply(self: List[sm.Term]): (List[sm.Term], Names) =
      UnzipReduce(self.map(this(_)))

    def apply(self: sm.Term): (sm.Term, Names) = self match

      case it @ sm.Term.Annotate(expr, annots) =>
        val (e, ens) = this(expr)
        val (as: List[sm.Mod.Annot], asns) = Stat.Mod(annots)
        it.copy(expr = e, annots = as) -> (ens ++ asns)

      case it @ sm.Term.AnonymousFunction(body) =>
        val (b, bns) = this(body)
        it.copy(body = b) -> bns

      case it @ sm.Term.Apply(fun, args) =>
        val (f, fns) = this(fun)
        val (as, asns) = this(args)
        it.copy(fun = f, args = as) -> (fns ++ asns)

      case it @ sm.Term.ApplyInfix(lhs, _, targs, args) =>
        val (l, lns) = this(lhs)
        val (ts, tsns) = Type(targs)
        val (as, asns) = this(args)
        it.copy(lhs = l, targs = ts, args = as) -> (lns ++ tsns ++ asns)

      case it @ sm.Term.ApplyType(fun, targs) =>
        val (f, fns) = this(fun)
        val (ts, tsns) = Type(targs)
        it.copy(fun = f, targs = ts) -> (fns ++ tsns)

      case it @ sm.Term.ApplyUsing(fun, args) =>
        val (f, fns) = this(fun)
        val (as, asns) = this(args)
        it.copy(fun = f, args = as) -> (fns ++ asns)

      case it @ sm.Term.Ascribe(expr, tpe) =>
        val (e, ens) = this(expr)
        val (t, tns) = Type(tpe)
        it.copy(expr = e, tpe = t) -> (ens ++ tns)

      case it @ sm.Term.Assign(lhs, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Term.Block(stats) =>
        val (ss, ssns) = Stat(stats)
        it.copy(stats = ss) -> ssns

      case it @ sm.Term.Do(body, expr) =>
        val (b, bns) = this(body)
        val (e, ens) = this(expr)
        it.copy(body = b, expr = e) -> (bns ++ ens)

      case it @ sm.Term.Eta(expr) =>
        val (e, ens) = this(expr)
        it.copy(expr = e) -> ens

      case it @ sm.Term.For(enums, body) =>
        val (es, esns) = Enumerator(enums)
        val (b, bns) = this(body)
        it.copy(enums = es, body = b) -> (esns ++ bns)

      case it @ sm.Term.ForYield(enums, body) =>
        val (es, esns) = Enumerator(enums)
        val (b, bns) = this(body)
        it.copy(enums = es, body = b) -> (esns ++ bns)

      case it @ sm.Term.ContextFunction(params, body) =>
        val (List(ps), psns) = Param(List(params))
        val (b, bns) = this(body)
        it.copy(params = ps, body = b) -> (psns ++ bns)

      case it @ sm.Term.Function(params, body) =>
        val (List(ps), psns) = Param(List(params))
        val (b, bns) = this(body)
        it.copy(params = ps, body = b) -> (psns ++ bns)

      case it @ sm.Term.If(cond, thenp, elsep) =>
        val (c, cns) = this(cond)
        val (t, tns) = this(thenp)
        val (e, ens) = this(elsep)
        it.copy(cond = c, thenp = t, elsep = e) -> (cns ++ tns ++ ens)

      case it @ sm.Term.Interpolate(_, _, args) =>
        val (as, asns) = this(args)
        it.copy(args = as) -> asns

      case it @ sm.Lit.Symbol(free @ Symbol(name)) =>
         sm.Term.Name(name) -> Set(free)

      case it @ sm.Term.Match(expr, cases) =>
        val (e, ens) = this(expr)
        val (cs, csns) = Case(cases)
        it.copy(expr = e, cases = cs) -> (ens ++ csns)

      case it @ sm.Term.New(init) =>
        val (i, ins) = Init(init)
        it.copy(init = i) -> ins

      case it @ sm.Term.NewAnonymous(templ) =>
        val (t, tns) = Template(templ)
        it.copy(templ = t) -> tns

      case it @ sm.Term.PartialFunction(cases) =>
        val (cs, csns) = Case(cases)
        it.copy(cases = cs) -> csns

      case it @ sm.Term.PolyFunction(tparams, body) =>
        val (ts, tsns) = Type.Param(tparams)
        val (b, bns) = Term(body)
        it.copy(tparams = ts, body = b) -> (tsns ++ bns)

      case it @ sm.Term.QuotedMacroExpr(body) =>
        val (b, bns) = this(body)
        it.copy(body = b) -> bns

      case it @ sm.Term.QuotedMacroType(tpe) =>
        val (t, tns) = Type(tpe)
        it.copy(tpe = t) -> tns

      case it: sm.Term.Ref =>
        Ref(it)

      case it @ sm.Term.Repeated(expr) =>
        val (e, ens) = this(expr)
        it.copy(expr = e) -> ens

      case it @ sm.Term.Return(expr) =>
        val (e, ens) = this(expr)
        it.copy(expr = e) -> ens

      case it @ sm.Term.SplicedMacroExpr(body) =>
        val (b, bns) = this(body)
        it.copy(body = b) -> bns

      case _: sm.Term.SplicedMacroPat => ???

      case it @ sm.Term.Throw(expr) =>
        val (e, ens) = this(expr)
        it.copy(expr = e) -> ens

      case it @ sm.Term.Try(expr, catchp, finallyp) =>
        val (e, ens) = this(expr)
        val (c, cns) = Case(catchp)
        val Some((f, fns)) = finallyp.map(this(_)).orElse(Some(`null -> Nil`))
        it.copy(expr = e, catchp = c, finallyp = finallyp.map(const(f))) -> (ens ++ cns ++ fns)

      case it @ sm.Term.TryWithHandler(expr, catchp, finallyp) =>
        val (e, ens) = this(expr)
        val (c, cns) = this(catchp)
        val Some((f, fns)) = finallyp.map(this(_)).orElse(Some(`null -> Nil`))
        it.copy(expr = e, catchp = c, finallyp = finallyp.map(const(f))) -> (ens ++ cns ++ fns)

      case it @ sm.Term.Tuple(args) =>
        val (as, asns) = this(args)
        it.copy(args = as) -> asns

      case it @ sm.Term.While(expr, body) =>
        val (e, ens) = this(expr)
        val (b, bns) = this(body)
        it.copy(expr = e, body = b) -> (ens ++ bns)

      case it @ sm.Term.Xml(_, args) =>
        val (as, asns) = this(args)
        it.copy(args = as) -> asns

      case it =>
        it -> Names()


    // P /////////////////////////////////////////////////////////////////////

    object Param:

      def apply(self: List[List[sm.Term.Param]]): (List[List[sm.Term.Param]], Names) =
        UnzipReduce(self.map { ls => UnzipReduce(ls.map(this(_))) })

      def apply(self: sm.Term.Param): (sm.Term.Param, Names) = self match

        case it @ sm.Term.Param(mods, _, decltpe, default) =>
          val (ms, msns) = Stat.Mod(mods)
          val Some((dt, dtns)) = decltpe.map(Type(_)).orElse(Some(Type.`null -> Nil`))
          val Some((d, dns)) = default.map(Term(_)).orElse(Some(Term.`null -> Nil`))
          it.copy(mods = ms, decltpe = decltpe.map(const(dt)), default = default.map(const(d))) -> (msns ++ dtns ++ dns)


    // R /////////////////////////////////////////////////////////////////////

    object Ref:

      def apply(self: sm.Term.Ref): (sm.Term.Ref, Names) = self match

        case it @ sm.Term.Select(qual, _) =>
          val (q, qns) = Term(qual)
          it.copy(qual = q) -> qns

        case it @ sm.Term.ApplyUnary(_, arg) =>
          val (a, ans) = Term(arg)
          it.copy(arg = a) -> ans

        case it =>
          it -> Names()


  object Type:

    val `null -> Nil` = null.asInstanceOf[sm.Type] -> Names()

    def apply(self: sm.Type.Bounds): (sm.Type.Bounds, Names) = self match

      case it @ sm.Type.Bounds(lo, hi) =>
        val Some((l, lns)) = lo.map(this(_)).orElse(Some(`null -> Nil`))
        val Some((h, hns)) = hi.map(this(_)).orElse(Some(`null -> Nil`))
        it.copy(lo = lo.map(const(l)), hi = hi.map(const(h))) -> (lns ++ hns)

    def apply(self: List[sm.Type]): (List[sm.Type], Names) =
      UnzipReduce(self.map(this(_)))

    def apply(self: sm.Type): (sm.Type, Names) = self match

      case it @ sm.Type.And(lhs, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Type.Annotate(tpe, annots) =>
        val (t, tns) = this(tpe)
        val (as: List[sm.Mod.Annot], asns) = Stat.Mod(annots)
        it.copy(tpe = t, annots = as) -> (tns ++ asns)

      case it @ sm.Type.AnonymousLambda(tpe) =>
        val (t, tns) = this(tpe)
        it.copy(tpe = t) -> tns

      case it @ sm.Type.Apply(tpe, args) =>
        val (t, tns) = this(tpe)
        val (as, asns) = this(args)
        it.copy(tpe = t, args = as) -> (tns ++ asns)

      case it @ sm.Type.ApplyInfix(lhs, _, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Type.Block(_, tpe) =>
        val (t, tns) = this(tpe)
        it.copy(tpe = t) -> tns

      case it @ sm.Type.ByName(tpe) =>
        val (t, tns) = this(tpe)
        it.copy(tpe = t) -> tns

      case it @ sm.Type.Existential(tpe, stats) =>
        val (t, tns) = this(tpe)
        val (ss, ssns) = Stat(stats)
        it.copy(tpe = t, stats = ss) -> (tns ++ ssns)

      case it @ sm.Type.FunctionArg(mods, tpe) =>
        val (ms, msns) = Stat.Mod(mods)
        val (t, tns) = this(tpe)
        it.copy(mods = ms, tpe = t) -> (msns ++ tns)

      case it @ sm.Type.TypedParam(_, typ) =>
        val (t, tns) = this(typ)
        it.copy(typ = t) -> tns

      case it @ sm.Type.ContextFunction(params, res) =>
        val (ps, psns) = this(params)
        val (r, rns) = this(res)
        it.copy(params = ps, res = r) -> (psns ++ rns)

      case it @ sm.Type.Function(params, res) =>
        val (ps, psns) = this(params)
        val (r, rns) = this(res)
        it.copy(params = ps, res = r) -> (psns ++ rns)

      case it @ sm.Type.ImplicitFunction(params, res) =>
        val (ps, psns) = this(params)
        val (r, rns) = this(res)
        it.copy(params = ps, res = r) -> (psns ++ rns)

      case it @ sm.Type.Lambda(tparams, tpe) =>
        val (ts, tsns) = Param(tparams)
        val (t, tns) = this(tpe)
        it.copy(tparams = ts, tpe = t) -> (tsns ++ tns)

      case _: sm.Lit => ???

      case it @ sm.Type.Macro(body) =>
        val (b, bns) = Term(body)
        it.copy(body = b) -> bns

      case it @ sm.Type.Match(tpe, cases) =>
        val (t, tns) = this(tpe)
        val (cs: List[sm.TypeCase], csns) = UnzipReduce(cases.map(CaseTree(_)))
        it.copy(tpe = t, cases = cs) -> (tns ++ csns)

      case it @ sm.Type.Method(paramss, tpe) =>
        val (ps, psns) = Term.Param(paramss)
        val (t, tns) = this(tpe)
        it.copy(paramss = ps, tpe = t) -> (psns ++ tns)

      case it @ sm.Type.Or(lhs, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it @ sm.Type.Wildcard(bounds) =>
        val (bs, bsns) = this(bounds)
        it.copy(bounds = bs) -> bsns

      case it @ sm.Type.PolyFunction(tparams, tpe) =>
        val (ts, tsns) = Param(tparams)
        val (t, tns) = this(tpe)
        it.copy(tparams = ts, tpe = t) -> (tsns ++ tns)

      case it: sm.Type.Ref =>
        Ref(it)

      case it @ sm.Type.Refine(tpe, stats) =>
        val Some((t, tns)) = tpe.map(this(_)).orElse(Some(`null -> Nil`))
        val (ss, ssns) = Stat(stats)
        it.copy(tpe = tpe.map(const(t)), stats = ss) -> (tns ++ ssns)

      case it @ sm.Type.Repeated(tpe) =>
        val (t, tns) = this(tpe)
        it.copy(tpe = t) -> tns

      case it @ sm.Type.Tuple(args) =>
        val (as, asns) = this(args)
        it.copy(args = as) -> asns

      case it @ sm.Type.With(lhs, rhs) =>
        val (l, lns) = this(lhs)
        val (r, rns) = this(rhs)
        it.copy(lhs = l, rhs = r) -> (lns ++ rns)

      case it =>
        it -> Names()


    // P /////////////////////////////////////////////////////////////////////

    object Param:

      def apply(self: List[sm.Type.Param]): (List[sm.Type.Param], Names) =
        UnzipReduce(self.map(this(_)))

      def apply(self: sm.Type.Param): (sm.Type.Param, Names) = self match

        case it @ sm.Type.Param(mods, _, tparams, tbounds, vbounds, cbounds) =>
          val (ms, msns) = Stat.Mod(mods)
          val (ts, tsns) = this(tparams)
          val (tbs, tbsns) = Type(tbounds)
          val (vbs, vbsns) = Type(vbounds)
          val (cbs, cbsns) = Type(cbounds)
          it.copy(mods = ms, tparams = ts, tbounds = tbs, vbounds = vbs, cbounds = cbs) -> (msns ++ tsns ++ tbsns ++ vbsns ++ cbsns)


    // R /////////////////////////////////////////////////////////////////////

    object Ref:

      def apply(self: sm.Type.Ref): (sm.Type.Ref, Names) = self match

        case it @ sm.Type.Project(qual, _) =>
          val (q, qns) = Type(qual)
          it.copy(qual = q) -> qns

        case it @ sm.Type.Select(qual, _) =>
          val (q, qns) = Term.Ref(qual)
          it.copy(qual = q) -> qns

        case it @ sm.Type.Singleton(ref) =>
          val (r, rns) = Term.Ref(ref)
          it.copy(ref = r) -> rns

        case it =>
          it -> Names()
