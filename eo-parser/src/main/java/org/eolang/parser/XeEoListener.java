/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
import org.cactoos.iterable.Mapped;
import org.cactoos.text.Joined;
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
     * The name of it.
     */
    private final String name;

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
     *
     * @param name The name of it
     */
    XeEoListener(final String name) {
        this.name = name;
        this.dirs = new Directives();
        this.errors = new ArrayList<>(0);
        this.objects = new Objects.ObjXembly();
        this.start = System.nanoTime();
    }

    @Override
    public void enterProgram(final EoParser.ProgramContext ctx) {
        this.dirs
            .append(new DrProgram(this.name))
            .append(new DrListing(ctx))
            .xpath("/program").strict(1);
    }

    @Override
    public void exitProgram(final EoParser.ProgramContext ctx) {
        this.dirs.xpath("/program").strict(1);
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
    public void enterLicense(final EoParser.LicenseContext ctx) {
        this.dirs.addIf("license").set(
            new Joined(
                "\n",
                new Mapped<>(
                    cmt -> cmt.getText().substring(1).trim(),
                    ctx.COMMENTARY()
                )
            )
        ).up();
    }

    @Override
    public void exitLicense(final EoParser.LicenseContext ctx) {
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
                this.dirs.set(pair[1].trim()).up();
                for (final String part : pair[1].trim().split(" ")) {
                    this.dirs.add("part").set(part).up();
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
    public void enterObjects(final EoParser.ObjectsContext ctx) {
        this.dirs.add("objects");
    }

    @Override
    public void exitObjects(final EoParser.ObjectsContext ctx) {
        this.dirs.append(this.objects).up();
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
    public void enterCommentMandatory(final EoParser.CommentMandatoryContext ctx) {
        this.putComment(ctx.comment(), ctx.getStop());
    }

    @Override
    public void exitCommentMandatory(final EoParser.CommentMandatoryContext ctx) {
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
    public void enterSlave(final EoParser.SlaveContext ctx) {
        // Nothing here
    }

    @Override
    public void exitSlave(final EoParser.SlaveContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMaster(final EoParser.MasterContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMaster(final EoParser.MasterContext ctx) {
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
    public void enterJust(final EoParser.JustContext ctx) {
        // Nothing here
    }

    @Override
    public void exitJust(final EoParser.JustContext ctx) {
        // Nothing here
    }

    @Override
    public void enterJustNamed(final EoParser.JustNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitJustNamed(final EoParser.JustNamedContext ctx) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterAtom(final EoParser.AtomContext ctx) {
        this.startObject(ctx)
            .prop("atom", ctx.type().typeFqn().getText())
            .leave();
    }

    @Override
    public void exitAtom(final EoParser.AtomContext ctx) {
        // Nothing here
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
    public void enterVoids(final EoParser.VoidsContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitVoids(final EoParser.VoidsContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterVoid(final EoParser.VoidContext ctx) {
        this.startObject(ctx);
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
    public void enterType(final EoParser.TypeContext ctx) {
        // Nothing here
    }

    @Override
    public void exitType(final EoParser.TypeContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTypeFqn(final EoParser.TypeFqnContext ctx) {
        // Nothing here
    }

    @Override
    public void exitTypeFqn(final EoParser.TypeFqnContext ctx) {
        // Nothing here
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
    public void enterHapplicationExtended(final EoParser.HapplicationExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationExtended(final EoParser.HapplicationExtendedContext ctx) {
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
        this.startObject(ctx);
        final String base;
        if (ctx.STAR() != null) {
            base = "tuple";
            this.objects.prop("star");
        } else if (ctx.NAME() != null) {
            base = ctx.NAME().getText();
        } else if (ctx.PHI() != null) {
            base = "@";
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
    public void enterHapplicationTail(final EoParser.HapplicationTailContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationTail(final EoParser.HapplicationTailContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterHapplicationTailReversed(final EoParser.HapplicationTailReversedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationTailReversed(final EoParser.HapplicationTailReversedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationTailReversedFirst(
        final EoParser.HapplicationTailReversedFirstContext ctx
    ) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationTailReversedFirst(
        final EoParser.HapplicationTailReversedFirstContext ctx
    ) {
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
    public void enterVapplication(final EoParser.VapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplication(final EoParser.VapplicationContext ctx) {
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
            if (num.charAt(0) == '+'
                || num.charAt(0) == '-'
                || num.length() > 1 && num.charAt(0) == '0'
                || Integer.parseInt(num) < 0
            ) {
                this.errors.add(
                    XeEoListener.error(
                        ctx,
                        "Index after '*' must be a positive integer without leading zero or arithmetic signs"
                    )
                );
            }
            final int number = Integer.parseInt(num);
            count = Math.max(number, 0);
        } else {
            count = 0;
        }
        this.startObject(ctx)
            .prop("base", ctx.NAME().getText())
            .prop("before-star", count);
    }

    @Override
    public void exitCompactArray(final EoParser.CompactArrayContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterVapplicationHeadNamed(final EoParser.VapplicationHeadNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationHeadNamed(final EoParser.VapplicationHeadNamedContext ctx) {
        // Nothing here
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
    public void enterProhibitedComment(final EoParser.ProhibitedCommentContext ctx) {
        this.errors.add(
            XeEoListener.error(ctx.comment().COMMENTARY(), "Comment here is prohibited")
        );
    }

    @Override
    public void exitProhibitedComment(final EoParser.ProhibitedCommentContext ctx) {
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
        // Nothing here
    }

    @Override
    public void exitVapplicationArgBoundNext(final EoParser.VapplicationArgBoundNextContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgUnbound(final EoParser.VapplicationArgUnboundContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitVapplicationArgUnbound(final EoParser.VapplicationArgUnboundContext ctx) {
        this.objects.leave();
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
    public void enterVapplicationArgHapplicationBound(
        final EoParser.VapplicationArgHapplicationBoundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHapplicationBound(
        final EoParser.VapplicationArgHapplicationBoundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgHapplicationUnbound(
        final EoParser.VapplicationArgHapplicationUnboundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHapplicationUnbound(
        final EoParser.VapplicationArgHapplicationUnboundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationHeadAs(
        final EoParser.VapplicationHeadAsContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationHeadAs(final EoParser.VapplicationHeadAsContext ctx) {
        // Nothing here
    }

    @Override
    public void enterFormationNameless(final EoParser.FormationNamelessContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitFormationNameless(final EoParser.FormationNamelessContext ctx) {
        // Nothing here
    }

    @Override
    public void enterFormationNamedOrNameless(final EoParser.FormationNamedOrNamelessContext ctx) {
        // Nothing here
    }

    @Override
    public void exitFormationNamedOrNameless(final EoParser.FormationNamedOrNamelessContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgVanonymBound(
        final EoParser.VapplicationArgVanonymBoundContext ctx
    ) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitVapplicationArgVanonymBound(
        final EoParser.VapplicationArgVanonymBoundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterAttributesAs(final EoParser.AttributesAsContext ctx) {
        // Nothing here
    }

    @Override
    public void exitAttributesAs(final EoParser.AttributesAsContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgHanonymBoundBody(
        final EoParser.VapplicationArgHanonymBoundBodyContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHanonymBoundBody(
        final EoParser.VapplicationArgHanonymBoundBodyContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgHanonymBound(
        final EoParser.VapplicationArgHanonymBoundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHanonymBound(
        final EoParser.VapplicationArgHanonymBoundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgHanonymUnbound(
        final EoParser.VapplicationArgHanonymUnboundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHanonymUnbound(
        final EoParser.VapplicationArgHanonymUnboundContext ctx
    ) {
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
        this.objects.enter().prop("name", "@").leave().leave();
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
    public void enterMethodNamed(final EoParser.MethodNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMethodNamed(final EoParser.MethodNamedContext ctx) {
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
    public void enterHmethodHead(final EoParser.HmethodHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodHead(final EoParser.HmethodHeadContext ctx) {
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
    public void enterMethodTailOptional(final EoParser.MethodTailOptionalContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMethodTailOptional(final EoParser.MethodTailOptionalContext ctx) {
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
        // Nothing here
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
        this.startObject(ctx);
        if (ctx.data() == null) {
            final String base;
            if (ctx.XI() != null) {
                base = "$";
            } else if (ctx.STAR() != null) {
                base = "tuple";
                this.objects.prop("star");
            } else if (ctx.ROOT() != null) {
                base = "Q";
            } else if (ctx.HOME() != null) {
                base = "QQ";
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
        this.startObject(ctx);
        final String base;
        if (ctx.NAME() != null) {
            base = ctx.NAME().getText();
        } else if (ctx.PHI() != null) {
            base = "@";
        } else if (ctx.RHO() != null) {
            base = "^";
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
        // Nothing here
    }

    @Override
    public void exitReversed(final EoParser.ReversedContext ctx) {
        this.objects.enter().xprop("base", "concat('.',@base)").leave();
    }

    @Override
    public void enterAname(final EoParser.AnameContext ctx) {
        this.objects
            .enter()
            .prop(
                "name",
                String.format(
                    "auto-named-attr-at-%d-%d",
                    ctx.getStart().getLine(),
                    ctx.getStart().getCharPositionInLine()
                )
            )
            .leave();
    }

    @Override
    public void exitAname(final EoParser.AnameContext ctx) {
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
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterSuffix(final EoParser.SuffixContext ctx) {
        this.objects.enter();
        if (ctx.PHI() != null) {
            this.objects.prop("name", ctx.PHI().getText());
        } else if (ctx.NAME() != null) {
            this.objects.prop("name", ctx.NAME().getText());
        }
    }

    @Override
    public void exitSuffix(final EoParser.SuffixContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterSpacedArrow(final EoParser.SpacedArrowContext ctx) {
        // Nothing here
    }

    @Override
    public void exitSpacedArrow(final EoParser.SpacedArrowContext ctx) {
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
            final int index = Integer.parseInt(ctx.INT().getText());
            if (index < 0) {
                this.errors.add(XeEoListener.error(ctx, "Object binding can't be negative"));
            }
            has = String.format("α%d", index);
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
        final Supplier<String> data;
        final String base;
        final String text = ctx.getText();
        if (ctx.BYTES() != null) {
            base = "bytes";
            data = () -> text.replaceAll("\\s+", "").trim();
        } else if (ctx.FLOAT() != null || ctx.INT() != null) {
            base = "number";
            data = new BytesToHex(
                ByteBuffer
                    .allocate(Double.BYTES)
                    .putDouble(Double.parseDouble(text))
                    .array()
            );
        } else if (ctx.HEX() != null) {
            base = "number";
            data = new BytesToHex(
                ByteBuffer
                    .allocate(Double.BYTES)
                    .putDouble(((Long) Long.parseLong(text.substring(2), 16)).doubleValue())
                    .array()
            );
        } else if (ctx.STRING() != null) {
            base = "string";
            data = new BytesToHex(
                StringEscapeUtils.unescapeJava(
                    text.substring(1, text.length() - 1)
                ).getBytes(StandardCharsets.UTF_8)
            );
        } else {
            base = "string";
            final int indent = ctx.getStart().getCharPositionInLine();
            data = new BytesToHex(
                StringEscapeUtils.unescapeJava(
                    XeEoListener.trimMargin(text, indent)
                ).getBytes(StandardCharsets.UTF_8)
            );
        }
        this.objects.prop("base", base).data(data.get());
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
     * Start object.
     *
     * @param ctx Context.
     * @return Started object.
     */
    private Objects startObject(final ParserRuleContext ctx) {
        return this.objects.start(
            ctx.getStart().getLine(),
            ctx.getStart().getCharPositionInLine()
        );
    }

    /**
     * Start abstract object.
     *
     * @param ctx Context
     * @return Xembly objects after creating abstract object
     */
    private Objects startAbstract(final ParserRuleContext ctx) {
        return this.startObject(ctx).leave();
    }

    /**
     * Build comment from context.
     * @param comment As they come from the parser
     * @param stop Stop line of the comment
     */
    private void putComment(final List<EoParser.CommentContext> comment, final Token stop) {
        if (!comment.isEmpty()) {
            this.dirs.push().xpath("/program").addIf("comments").add("comment").set(
                comment.stream().map(
                    context -> context.COMMENTARY().getText().substring(1).trim()
                ).collect(Collectors.joining("\\n"))
            ).attr("line", stop.getLine() + 1).pop();
        }
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
     * Create parsing exception from given context.
     * @param ctx Context
     * @param msg Error message
     * @return Parsing exception from the current context
     */
    private static ParsingException error(final ParserRuleContext ctx, final String msg) {
        return new ParsingException(
            ctx.getStart().getLine(),
            new MsgLocated(
                ctx.getStart().getLine(),
                ctx.getStart().getCharPositionInLine(),
                msg
            ).formatted(),
            new MsgUnderlined(
                XeEoListener.line(ctx),
                ctx.getStart().getCharPositionInLine(),
                ctx.getText().length()
            ).formatted()
        );
    }

    /**
     * Create parsing exception from given terminal node.
     * @param terminal Terminal node
     * @param msg Error message
     * @return Parsing exception from the current terminal node
     */
    private static ParsingException error(final TerminalNode terminal, final String msg) {
        return new ParsingException(
            terminal.getSymbol().getLine(),
            new MsgLocated(
                terminal.getSymbol().getLine(),
                terminal.getSymbol().getCharPositionInLine(),
                msg
            ).formatted(),
            new MsgUnderlined(
                XeEoListener.line(terminal.getSymbol()),
                terminal.getSymbol().getCharPositionInLine(),
                terminal.getText().length()
            ).formatted()
        );
    }

    /**
     * Get line from context.
     * @param ctx Context
     * @return Line
     */
    private static String line(final ParserRuleContext ctx) {
        return XeEoListener.line(ctx.start);
    }

    /**
     * Get line from token.
     * @param token Token
     * @return Line
     */
    private static String line(final Token token) {
        final int number = token.getLine();
        final String[] lines = token.getInputStream().toString().split("\n");
        if (number > 0 && number <= lines.length) {
            return lines[number - 1];
        } else {
            throw new IllegalArgumentException(
                String.format(
                    "Line number '%s' out of bounds, total lines: %d",
                    number,
                    lines.length
                )
            );
        }
    }
}
