/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.text.StringEscapeUtils;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * The EO grammar listener for ANTLR4 walker.
 *
 * @checkstyle CyclomaticComplexityCheck (500 lines)
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 * @checkstyle MethodCountCheck (1300 lines)
 * @since 0.1
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.AvoidDuplicateLiterals",
    "PMD.ExcessivePublicCount",
    "PMD.ExcessiveClassLength",
    "PMD.GodClass"
})
final class XeEoListener implements EoListener, Iterable<Directive> {
    /**
     * Xembly directives we are building (mutable).
     */
    private final Directives dirs;

    /**
     * Xembly directives for objects (mutable).
     */
    private final Objects objects;

    /**
     * When we start.
     */
    private final long start;

    /**
     * Errors.
     */
    private final List<ParsingException> errors;

    /**
     * Ctor.
     */
    XeEoListener() {
        this.dirs = new Directives();
        this.errors = new ArrayList<>(0);
        this.objects = new Objects();
        this.start = System.nanoTime();
    }

    @Override
    public void enterProgram(final EoParser.ProgramContext ctx) {
        this.dirs
            .append(new DrProgram())
            .append(new DrListing(ctx))
            .xpath("/object")
            .strict(1);
    }

    @Override
    public void exitProgram(final EoParser.ProgramContext ctx) {
        this.dirs.xpath("/object")
            .strict(1)
            .append(this.objects);
        if (!this.errors.isEmpty()) {
            this.dirs.append(new DrErrors(this.errors));
        }
        this.dirs
            .attr("ms", (System.nanoTime() - this.start) / (1000L * 1000L))
            .up();
    }

    @Override
    public void enterEop(final EoParser.EopContext ctx) {
        // Nothing here
    }

    @Override
    public void exitEop(final EoParser.EopContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMetas(final EoParser.MetasContext ctx) {
        this.dirs.addIf("metas");
        for (final TerminalNode node : ctx.META()) {
            final String[] pair = node.getText().split(" ", 2);
            final String head = pair[0].substring(1);
            this.dirs.add("meta")
                .attr("line", node.getSymbol().getLine())
                .add("head").set(head).up()
                .add("tail");
            if (pair.length > 1) {
                this.dirs.set(XeEoListener.qqToGlobalPhi(pair[1].trim())).up();
                for (final String part : pair[1].trim().split(" ")) {
                    this.dirs.add("part").set(XeEoListener.qqToGlobalPhi(part)).up();
                }
            } else {
                this.dirs.up();
            }
            this.dirs.up();
        }
        this.dirs.up();
    }

    @Override
    public void exitMetas(final EoParser.MetasContext ctx) {
        // Nothing here
    }

    @Override
    public void enterComment(final EoParser.CommentContext ctx) {
        // Nothing here
    }

    @Override
    public void exitComment(final EoParser.CommentContext ctx) {
        // Nothing here
    }

    @Override
    public void enterCommentOptional(final EoParser.CommentOptionalContext ctx) {
        this.putComment(ctx.comment(), ctx.getStop());
    }

    @Override
    public void exitCommentOptional(final EoParser.CommentOptionalContext ctx) {
        // Nothing here
    }

    @Override
    public void enterObject(final EoParser.ObjectContext ctx) {
        // Nothing here
    }

    @Override
    public void exitObject(final EoParser.ObjectContext ctx) {
        // Nothing here
    }

    @Override
    public void enterBound(final EoParser.BoundContext ctx) {
        // Nothing here
    }

    @Override
    public void exitBound(final EoParser.BoundContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTbound(final EoParser.TboundContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTbound(final EoParser.TboundContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTsubMaster(final EoParser.TsubMasterContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTsubMaster(final EoParser.TsubMasterContext ctx) {
        // Nothing here
    }

    @Override
    public void enterSubMaster(final EoParser.SubMasterContext ctx) {
        // Nothing here
    }

    @Override
    public void exitSubMaster(final EoParser.SubMasterContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMasterBody(final EoParser.MasterBodyContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMasterBody(final EoParser.MasterBodyContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTmasterBody(final EoParser.TmasterBodyContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTmasterBody(final EoParser.TmasterBodyContext ctx) {
        // Nothing here
    }

    @Override
    public void enterJust(final EoParser.JustContext ctx) {
        // Nothing here
    }

    @Override
    public void exitJust(final EoParser.JustContext ctx) {
        // Nothing here
    }

    @Override
    public void enterAtom(final EoParser.AtomContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitAtom(final EoParser.AtomContext ctx) {
        this.objects.enter()
            .start(ctx.getStart().getLine(), ctx.getStop().getCharPositionInLine() - 1)
            .prop("name", "λ")
            .leave()
            .leave();
    }

    @Override
    public void enterFormation(final EoParser.FormationContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitFormation(final EoParser.FormationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTformation(final EoParser.TformationContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitTformation(final EoParser.TformationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTestsOrEol(final EoParser.TestsOrEolContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTestsOrEol(final EoParser.TestsOrEolContext ctx) {
        // Nothing here
    }

    @Override
    public void enterInnersOrEol(final EoParser.InnersOrEolContext ctx) {
        // Nothing here
    }

    @Override
    public void exitInnersOrEol(final EoParser.InnersOrEolContext ctx) {
        // Nothing here
    }

    @Override
    public void enterInners(final EoParser.InnersContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitInners(final EoParser.InnersContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterTests(final EoParser.TestsContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitTests(final EoParser.TestsContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterVoids(final EoParser.VoidsContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitVoids(final EoParser.VoidsContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterVoid(final EoParser.VoidContext ctx) {
        this.objects.start(ctx);
        final String name;
        if (ctx.NAME() != null) {
            name = ctx.NAME().getText();
        } else if (ctx.PHI() != null) {
            name = ctx.PHI().getText();
        } else {
            name = "";
        }
        if (!name.isEmpty()) {
            this.objects.prop("name", name);
        }
        this.objects.prop("base", "∅");
    }

    @Override
    public void exitVoid(final EoParser.VoidContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterApplication(final EoParser.ApplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitApplication(final EoParser.ApplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTapplication(final EoParser.TapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTapplication(final EoParser.TapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationExtended(final EoParser.HapplicationExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationExtended(final EoParser.HapplicationExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterOnlyAphi(final EoParser.OnlyAphiContext ctx) {
        this.startAutoPhiFormation(ctx, ctx.happlicationHeadExtended().getText());
    }

    @Override
    public void exitOnlyAphi(final EoParser.OnlyAphiContext ctx) {
        this.objects.leave().leave();
    }

    @Override
    public void enterAphi(final EoParser.AphiContext ctx) {
        // Nothing here
    }

    @Override
    public void exitAphi(final EoParser.AphiContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationReversed(final EoParser.HapplicationReversedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationReversed(final EoParser.HapplicationReversedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplication(final EoParser.HapplicationContext ctx) {
        // Nothing here
    }

    @Override
        public void exitHapplication(final EoParser.HapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationReversedHead(final EoParser.HapplicationReversedHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationReversedHead(final EoParser.HapplicationReversedHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationHead(final EoParser.HapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationHead(final EoParser.HapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationHeadExtended(
        final EoParser.HapplicationHeadExtendedContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitHapplicationHeadExtended(
        final EoParser.HapplicationHeadExtendedContext ctx
    ) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterApplicable(final EoParser.ApplicableContext ctx) {
        this.objects.start(ctx);
        final String base;
        if (ctx.STAR() != null) {
            base = "Φ.org.eolang.tuple";
            this.objects.prop("star");
        } else if (ctx.NAME() != null) {
            base = ctx.NAME().getText();
        } else if (ctx.PHI() != null) {
            base = "φ";
        } else {
            base = "";
        }
        if (!base.isEmpty()) {
            this.objects.prop("base", base);
        }
        this.objects.leave();
    }

    @Override
    public void exitApplicable(final EoParser.ApplicableContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationArgUnbound(final EoParser.HapplicationArgUnboundContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationArgUnbound(final EoParser.HapplicationArgUnboundContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationTailScoped(final EoParser.HapplicationTailScopedContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationTailScoped(final EoParser.HapplicationTailScopedContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterHapplicationTail(final EoParser.HapplicationTailContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationTail(final EoParser.HapplicationTailContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterHapplicationReversedFirst(
        final EoParser.HapplicationReversedFirstContext ctx
    ) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationReversedFirst(final EoParser.HapplicationReversedFirstContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterHapplicationArg(final EoParser.HapplicationArgContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationArg(final EoParser.HapplicationArgContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationArgScoped(final EoParser.HapplicationArgScopedContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitHapplicationArgScoped(final EoParser.HapplicationArgScopedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplication(final EoParser.VapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplication(final EoParser.VapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTvapplication(final EoParser.TvapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTvapplication(final EoParser.TvapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationHead(final EoParser.VapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationHead(final EoParser.VapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterCompactArray(final EoParser.CompactArrayContext ctx) {
        final int count;
        if (ctx.INT() != null) {
            final String num = ctx.INT().getText();
            final int number = Integer.parseInt(num);
            if (num.charAt(0) == '+'
                || num.charAt(0) == '-'
                || num.length() > 1 && num.charAt(0) == '0'
                || number < 0
            ) {
                this.errors.add(
                    new ParsingError(
                        ctx,
                        "Index after '*' must be a positive integer without leading zero or arithmetic signs"
                    ).cause()
                );
            }
            count = Math.max(number, 0);
        } else {
            count = 0;
        }
        this.objects.start(ctx)
            .prop("base", new CompactArrayFqn(ctx).asString())
            .prop("before-star", count);
    }

    @Override
    public void exitCompactArray(final EoParser.CompactArrayContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterVapplicationArgs(final EoParser.VapplicationArgsContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgs(final EoParser.VapplicationArgsContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgsReversed(final EoParser.VapplicationArgsReversedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgsReversed(final EoParser.VapplicationArgsReversedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgsSpecific(final EoParser.VapplicationArgsSpecificContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgsSpecific(final EoParser.VapplicationArgsSpecificContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgBound(final EoParser.VapplicationArgBoundContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitVapplicationArgBound(final EoParser.VapplicationArgBoundContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterVapplicationArgBoundCurrent(
        final EoParser.VapplicationArgBoundCurrentContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgBoundCurrent(
        final EoParser.VapplicationArgBoundCurrentContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgBoundNext(final EoParser.VapplicationArgBoundNextContext ctx) {
        if (ctx.commentOptional() != null && ctx.voids() != null) {
            this.startAbstract(ctx);
        }
    }

    @Override
    public void exitVapplicationArgBoundNext(final EoParser.VapplicationArgBoundNextContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgUnbound(final EoParser.VapplicationArgUnboundContext ctx) {
        if (ctx.aphi() != null) {
            final EoParser.VapplicationArgUnboundCurrentContext vertical =
                ctx.vapplicationArgUnboundCurrent();
            if (vertical.just() != null || vertical.method() != null) {
                this.startAutoPhiFormation(ctx, XeEoListener.verticalApplicationBase(vertical));
            }
        } else {
            this.objects.enter();
        }
    }

    @Override
    public void exitVapplicationArgUnbound(final EoParser.VapplicationArgUnboundContext ctx) {
        if (ctx.aphi() != null
            && (ctx.vapplicationArgUnboundCurrent().just() != null
            || ctx.vapplicationArgUnboundCurrent().method() != null)) {
            this.objects.leave().leave();
        } else {
            this.objects.leave();
        }
    }

    @Override
    public void enterVapplicationArgUnboundCurrent(
        final EoParser.VapplicationArgUnboundCurrentContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgUnboundCurrent(
        final EoParser.VapplicationArgUnboundCurrentContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgUnboundNext(
        final EoParser.VapplicationArgUnboundNextContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgUnboundNext(
        final EoParser.VapplicationArgUnboundNextContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterFormationNamed(final EoParser.FormationNamedContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitFormationNamed(final EoParser.FormationNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHformation(final EoParser.HformationContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitHformation(final EoParser.HformationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHanonym(final EoParser.HanonymContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHanonym(final EoParser.HanonymContext ctx) {
        // Nothing here
    }

    @Override
    public void enterOnlyphi(final EoParser.OnlyphiContext ctx) {
        this.startAbstract(ctx).enter();
    }

    @Override
    public void exitOnlyphi(final EoParser.OnlyphiContext ctx) {
        // Nothing here
    }

    @Override
    public void enterOnlyphiTail(final EoParser.OnlyphiTailContext ctx) {
        this.objects.enter().prop("name", "φ").leave().leave();
    }

    @Override
    public void exitOnlyphiTail(final EoParser.OnlyphiTailContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHanonymInner(final EoParser.HanonymInnerContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHanonymInner(final EoParser.HanonymInnerContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterMethod(final EoParser.MethodContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMethod(final EoParser.MethodContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethod(final EoParser.HmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethod(final EoParser.HmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethod(final EoParser.VmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethod(final EoParser.VmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodHead(final EoParser.VmethodHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodHead(final EoParser.VmethodHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodHeadApplicationTail(
        final EoParser.VmethodHeadApplicationTailContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVmethodHeadApplicationTail(
        final EoParser.VmethodHeadApplicationTailContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVmethodHeadVapplication(final EoParser.VmethodHeadVapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodHeadVapplication(final EoParser.VmethodHeadVapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMethodTail(final EoParser.MethodTailContext ctx) {
        if (ctx.INT() != null) {
            this.objects.start(ctx)
                .prop("base", this.alphaAttr(ctx, "Position of taken object can't be negative"))
                .leave();
        }
    }

    @Override
    public void exitMethodTail(final EoParser.MethodTailContext ctx) {
        this.objects
            .enter()
            .prop("method")
            .xprop("base", "concat('.',@base)")
            .xprop("pos", "@pos - 1")
            .leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterBeginner(final EoParser.BeginnerContext ctx) {
        this.objects.start(ctx);
        if (ctx.data() == null) {
            final String base;
            if (ctx.XI() != null) {
                base = "ξ";
            } else if (ctx.STAR() != null) {
                base = "Φ.org.eolang.tuple";
                this.objects.prop("star");
            } else if (ctx.ROOT() != null) {
                base = "Φ";
            } else if (ctx.HOME() != null) {
                base = "Φ̇";
            } else {
                base = "";
            }
            if (!base.isEmpty()) {
                this.objects.prop("base", base);
            }
        }
    }

    @Override
    public void exitBeginner(final EoParser.BeginnerContext ctx) {
        this.objects.leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterFinisher(final EoParser.FinisherContext ctx) {
        this.objects.start(ctx);
        final String base;
        if (ctx.NAME() != null) {
            base = ctx.NAME().getText();
        } else if (ctx.PHI() != null) {
            base = "φ";
        } else if (ctx.RHO() != null) {
            base = "ρ";
        } else {
            base = "";
        }
        if (!base.isEmpty()) {
            this.objects.prop("base", base);
        }
    }

    @Override
    public void exitFinisher(final EoParser.FinisherContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterReversed(final EoParser.ReversedContext ctx) {
        if (ctx.INT() != null) {
            this.objects.start(ctx)
                .prop("base", this.alphaAttr(ctx, "Position of taken object can't be negative"))
                .leave();
        }
    }

    @Override
    public void exitReversed(final EoParser.ReversedContext ctx) {
        this.objects.enter().xprop("base", "concat('.',@base)").leave();
    }

    @Override
    public void enterAname(final EoParser.AnameContext ctx) {
        this.objects.enter().prop("name", new AutoName(ctx).asString());
        if (ctx.CONST() != null) {
            this.objects.prop("const");
        }
        this.objects.leave();
    }

    @Override
    public void exitAname(final EoParser.AnameContext ctx) {
        // Nothing here
    }

    @Override
    public void enterFname(final EoParser.FnameContext ctx) {
        // Nothing here
    }

    @Override
    public void exitFname(final EoParser.FnameContext ctx) {
        // Nothing here
    }

    @Override
    public void enterOnameOrTname(final EoParser.OnameOrTnameContext ctx) {
        // Nothing here
    }

    @Override
    public void exitOnameOrTname(final EoParser.OnameOrTnameContext ctx) {
        // Nothing here
    }

    @Override
    public void enterOname(final EoParser.OnameContext ctx) {
        // Nothing here
    }

    @Override
    public void exitOname(final EoParser.OnameContext ctx) {
        if (ctx.CONST() != null) {
            this.objects.enter().prop("const").leave();
        }
    }

    @Override
    public void enterTname(final EoParser.TnameContext ctx) {
        this.objects.enter();
        if (ctx.PHI() != null) {
            this.objects.prop("name", ctx.PHI().getText());
        } else if (ctx.NAME() != null) {
            this.objects.prop(
                "name",
                String.format("%s%s", ctx.tarrow().PLUS().getText(), ctx.NAME().getText())
            );
        }
    }

    @Override
    public void exitTname(final EoParser.TnameContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterTarrow(final EoParser.TarrowContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTarrow(final EoParser.TarrowContext ctx) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterSuffix(final EoParser.SuffixContext ctx) {
        this.objects.enter();
        if (ctx.PHI() != null) {
            this.objects.prop("name", "φ");
        } else if (ctx.NAME() != null) {
            this.objects.prop("name", ctx.NAME().getText());
        }
        if (ctx.APOSTROPHE() != null) {
            this.objects.start(ctx).prop("base", "ξ.xi🌵").leave();
        }
    }

    @Override
    public void exitSuffix(final EoParser.SuffixContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterArrow(final EoParser.ArrowContext ctx) {
        // Nothing here
    }

    @Override
    public void exitArrow(final EoParser.ArrowContext ctx) {
        // Nothing here
    }

    @Override
    public void enterScope(final EoParser.ScopeContext ctx) {
        // Nothing here
    }

    @Override
    public void exitScope(final EoParser.ScopeContext ctx) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterAs(final EoParser.AsContext ctx) {
        this.objects.enter();
        final String has;
        if (ctx.NAME() != null) {
            has = ctx.NAME().getText();
        } else {
            has = this.alphaAttr(ctx, "Object binding can't be negative");
        }
        this.objects.prop("as", has);
    }

    @Override
    public void exitAs(final EoParser.AsContext ctx) {
        this.objects.leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterData(final EoParser.DataContext ctx) {
        final String text = ctx.getText();
        if (ctx.BYTES() != null) {
            this.objects
                .prop("base", "Φ.org.eolang.bytes")
                .start(ctx)
                .data(text.replaceAll("\\s+", "").trim())
                .leave();
        } else {
            final Supplier<String> data;
            final String base;
            if (ctx.FLOAT() != null || ctx.INT() != null) {
                base = "Φ.org.eolang.number";
                data = new BytesToHex(
                    ByteBuffer
                        .allocate(Double.BYTES)
                        .putDouble(Double.parseDouble(text))
                        .array()
                );
            } else if (ctx.HEX() != null) {
                base = "Φ.org.eolang.number";
                data = new BytesToHex(
                    ByteBuffer
                        .allocate(Double.BYTES)
                        .putDouble(((Long) Long.parseLong(text.substring(2), 16)).doubleValue())
                        .array()
                );
            } else if (ctx.STRING() != null) {
                base = "Φ.org.eolang.string";
                data = new BytesToHex(
                    StringEscapeUtils.unescapeJava(
                        text.substring(1, text.length() - 1)
                    ).getBytes(StandardCharsets.UTF_8)
                );
            } else {
                base = "Φ.org.eolang.string";
                final int indent = ctx.getStart().getCharPositionInLine();
                data = new BytesToHex(
                    StringEscapeUtils.unescapeJava(
                        XeEoListener.trimMargin(text, indent)
                    ).getBytes(StandardCharsets.UTF_8)
                );
            }
            this.objects
                .prop("base", base)
                .start(ctx)
                .prop("base", "Φ.org.eolang.bytes")
                .start(ctx)
                .data(data.get())
                .leave()
                .leave();
        }
    }

    @Override
    public void exitData(final EoParser.DataContext ctx) {
        // Nothing here
    }

    @Override
    public void visitTerminal(final TerminalNode node) {
        // This method is created by ANTLR and can't be removed
    }

    // We don't do anything here. We let the error nodes stay in the
    // tree. Later the syntax analysis will hit them and raise
    // ParsingException, with proper information about them. Here we
    // don't do anything, to not pollute the error reporting with
    // duplicated.
    @Override
    public void visitErrorNode(final ErrorNode node) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterEveryRule(final ParserRuleContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void exitEveryRule(final ParserRuleContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public Iterator<Directive> iterator() {
        return this.dirs.iterator();
    }

    /**
     * Start abstract object.
     *
     * @param ctx Context
     * @return Xembly objects after creating abstract object
     */
    private Objects startAbstract(final ParserRuleContext ctx) {
        return this.objects.start(ctx).leave();
    }

    /**
     * Build comment from context.
     * @param comment As they come from the parser
     * @param stop Stop line of the comment
     */
    private void putComment(final List<EoParser.CommentContext> comment, final Token stop) {
        if (!comment.isEmpty()) {
            this.dirs.push().xpath("/object").addIf("comments").add("comment").set(
                comment.stream().map(
                    context -> {
                        final String result;
                        final String text = context.COMMENTARY().getText();
                        if (text.isEmpty()) {
                            result = "";
                        } else {
                            final int sub;
                            if (text.length() > 1 && text.charAt(1) == ' ') {
                                sub = 2;
                            } else {
                                sub = 1;
                            }
                            result = context.COMMENTARY().getText().substring(sub);
                        }
                        return result;
                    }
                ).collect(Collectors.joining("\\n"))
            ).attr("line", stop.getLine() + 1).pop();
        }
    }

    /**
     * Extract positive integer from context and convert to alpha attribute.
     * @param ctx Context
     * @param msg Error message for the case if number is not positive
     * @return Formatted alpha attribute
     */
    private String alphaAttr(final ParserRuleContext ctx, final String msg) {
        final int index = Integer.parseInt(ctx.getToken(EoParser.INT, 0).getText());
        if (index < 0) {
            this.errors.add(new ParsingError(ctx, msg).cause());
        }
        return String.format("α%d", index);
    }

    /**
     * Start new Auto Phi formation, from an application.
     * @param ctx Context
     * @param application Application base
     */
    private void startAutoPhiFormation(final ParserRuleContext ctx, final String application) {
        this.startAbstract(ctx)
            .enter().prop("name", new AutoName(ctx).asString())
            .start(ctx)
            .prop("base", String.format("ξ.ρ.%s", application))
            .prop("name", "φ");
    }

    /**
     * Trim margin from text block.
     *
     * @param text Text block.
     * @param indent Indentation level.
     * @return Trimmed text.
     */
    private static String trimMargin(final String text, final int indent) {
        final String rexp = "[\\s]{%d}";
        final String cutted = text
            .substring(3, text.length() - 3).trim();
        final String[] splitted = cutted.split("\n");
        StringBuilder res = new StringBuilder();
        for (final String line : splitted) {
            res.append(line.replaceAll(String.format(rexp, indent), "")).append('\n');
        }
        if (res.length() > 0 && res.charAt(0) == '\n') {
            res = new StringBuilder(res.substring(1));
        }
        if (res.length() > 0 && res.charAt(res.length() - 1) == '\n') {
            res = new StringBuilder(res.substring(0, res.length() - 1));
        }
        return res.toString();
    }

    /**
     * Vertical application base.
     * @param ctx Context
     * @return The base of vertical application
     */
    private static String verticalApplicationBase(
        final EoParser.VapplicationArgUnboundCurrentContext ctx
    ) {
        final String result;
        if (ctx.method() != null) {
            result = ctx.method().getText();
        } else {
            result = ctx.just().getText();
        }
        return result;
    }

    /**
     * Translate FQN starting with `Q` or `QQ` to the one starting with a global Phi object.
     * @param fqn FQN
     * @return Translated FQN.
     */
    private static String qqToGlobalPhi(final String fqn) {
        final String result;
        if (fqn.startsWith("QQ")) {
            result = String.format("Φ̇%s", fqn.substring(2));
        } else if (!fqn.startsWith("QQ") && !fqn.isEmpty() && fqn.charAt(0) == 'Q') {
            result = String.format("Φ%s", fqn.substring(1));
        } else {
            result = fqn;
        }
        return result;
    }
}
