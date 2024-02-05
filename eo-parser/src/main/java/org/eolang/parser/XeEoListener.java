/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

import com.jcabi.manifests.Manifests;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Iterator;
import java.util.StringJoiner;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.text.StringEscapeUtils;
import org.cactoos.iterable.Mapped;
import org.cactoos.text.Joined;
import org.eolang.parser.xmir.XmirInfo;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * The EO grammar listener for ANTLR4 walker.
 *
 * @checkstyle CyclomaticComplexityCheck (500 lines)
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 * @checkstyle MethodCountCheck (1300 lines)
 * @since 0.1
 * @todo #2841:30min Change severity on comments validation. Current severity on comments validation
 *  is "warning". We need to change it to "error" to prevent users from ignoring this type of error.
 *  But firstly we have to make "eo-runtime" documented well. After it's done - we need to turn
 *  on "failOnWarning" trigger in pom.xml inside "eo-runtime.
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.AvoidDuplicateLiterals",
    "PMD.ExcessivePublicCount",
    "PMD.ExcessiveClassLength"
})
public final class XeEoListener implements EoListener, Iterable<Directive> {
    /**
     * Info about xmir.
     */
    private static final XmirInfo INFO = new XmirInfo();

    /**
     * Minimum allowed comment length.
     */
    @SuppressWarnings("PMD.LongVariable")
    private static final int MIN_COMMENT_LENGTH = 64;

    /**
     * Meta for testing.
     */
    private static final String TESTS_META = "tests";

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
     * Xembly directives to build errors.
     */
    private final Directives errors;

    /**
     * When we start.
     */
    private final long start;

    /**
     * If metas has "+tests" meta.
     */
    private boolean tests;

    /**
     * Ctor.
     *
     * @param name The name of it
     */
    public XeEoListener(final String name) {
        this.name = name;
        this.dirs = new Directives();
        this.errors = new Directives();
        this.objects = new Objects.ObjXembly();
        this.start = System.nanoTime();
    }

    @Override
    public void enterProgram(final EoParser.ProgramContext ctx) {
        this.dirs.add("program")
            .attr("name", this.name)
            .attr("version", Manifests.read("EO-Version"))
            .attr("revision", Manifests.read("EO-Revision"))
            .attr("dob", Manifests.read("EO-Dob"))
            .attr(
                "time",
                ZonedDateTime.now(ZoneOffset.UTC).format(
                    DateTimeFormatter.ISO_INSTANT
                )
            )
            .comment(XeEoListener.INFO)
            .add("listing").set(new SourceText(ctx)).up()
            .add("errors").up()
            .add("sheets").up()
            .add("license").up()
            .add("metas").up();
    }

    @Override
    public void exitProgram(final EoParser.ProgramContext ctx) {
        this.dirs
            .xpath("/program/errors")
            .append(this.errors).up()
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
            if (head.equals(XeEoListener.TESTS_META)) {
                this.tests = true;
            }
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
        // Nothing here
    }

    @Override
    public void exitCommentOptional(final EoParser.CommentOptionalContext ctx) {
        // Nothing here
    }

    @Override
    public void enterCommentMandatory(final EoParser.CommentMandatoryContext ctx) {
        if (!this.tests) {
            final String comment = String.join(
                "",
                ctx.comment().COMMENTARY().getText().substring(1).trim(),
                ctx.commentOptional().comment().stream().map(
                    context -> context.COMMENTARY().getText().substring(1).trim()
                ).collect(Collectors.joining(""))
            );
            final String length = String.format(
                "Comment must be at least %d characters long",
                XeEoListener.MIN_COMMENT_LENGTH
            );
            final String warning = "warning";
            if (comment.isEmpty()) {
                this.addError(ctx, "comment-length-check", warning, length);
            } else {
                if (comment.length() < XeEoListener.MIN_COMMENT_LENGTH) {
                    this.addError(ctx, "comment-length-check", warning, length);
                }
                if (comment.chars().anyMatch(chr -> chr < 32 || chr > 127)) {
                    this.addError(
                        ctx,
                        "comment-content-check",
                        warning,
                        "Comment must contain only ASCII printable characters: 0x20-0x7f"
                    );
                }
                if (!Character.isUpperCase(comment.charAt(0))) {
                    this.addError(
                        ctx,
                        "comment-start-character-check",
                        warning,
                        "Comment must start with capital letter"
                    );
                }
                if (comment.charAt(comment.length() - 1) != '.') {
                    this.addError(
                        ctx,
                        "comment-ending-check",
                        warning,
                        "Comment must end with dot"
                    );
                }
            }
        }
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
        this.startObject(ctx);
        if (ctx.type().NAME() != null) {
            this.objects.prop("atom", ctx.type().NAME().getText());
        } else if (ctx.type().QUESTION() != null) {
            this.objects.prop("atom", ctx.type().QUESTION().getText());
        }
        this.objects.leave();
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
    public void enterAttributes(final EoParser.AttributesContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitAttributes(final EoParser.AttributesContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterAttribute(final EoParser.AttributeContext ctx) {
        this.startObject(ctx).prop("name", ctx.NAME().getText());
    }

    @Override
    public void exitAttribute(final EoParser.AttributeContext ctx) {
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
        } else if (ctx.AT() != null) {
            base = "@";
        } else {
            base = "";
        }
        if (!base.isEmpty()) {
            this.objects.prop("base", base);
        }
        if (ctx.COPY() != null) {
            this.objects.prop("copy");
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
    public void enterHapplicationTailReversedFirst(final EoParser.HapplicationTailReversedFirstContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationTailReversedFirst(final EoParser.HapplicationTailReversedFirstContext ctx) {
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
    public void enterVapplicationArgBoundCurrent(final EoParser.VapplicationArgBoundCurrentContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgBoundCurrent(final EoParser.VapplicationArgBoundCurrentContext ctx) {
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
    public void enterVapplicationArgUnboundCurrent(final EoParser.VapplicationArgUnboundCurrentContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgUnboundCurrent(final EoParser.VapplicationArgUnboundCurrentContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgUnboundNext(final EoParser.VapplicationArgUnboundNextContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgUnboundNext(final EoParser.VapplicationArgUnboundNextContext ctx) {
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
    public void enterVapplicationArgVanonymUnbound(
        final EoParser.VapplicationArgVanonymUnboundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgVanonymUnbound(
        final EoParser.VapplicationArgVanonymUnboundContext ctx
    ) {
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
    public void enterVapplicationArgVanonymBound(
        final EoParser.VapplicationArgVanonymBoundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgVanonymBound(
        final EoParser.VapplicationArgVanonymBoundContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterFormationBound(final EoParser.FormationBoundContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitFormationBound(final EoParser.FormationBoundContext ctx) {
        // Nothing here
    }

    @Override
    public void enterFormationBoundNameless(final EoParser.FormationBoundNamelessContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitFormationBoundNameless(final EoParser.FormationBoundNamelessContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgHanonymBoundBody(final EoParser.VapplicationArgHanonymBoundBodyContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHanonymBoundBody(final EoParser.VapplicationArgHanonymBoundBodyContext ctx) {
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
    public void enterHanonym(final EoParser.HanonymContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitHanonym(final EoParser.HanonymContext ctx) {
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
    public void enterHmethodOptional(final EoParser.HmethodOptionalContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodOptional(final EoParser.HmethodOptionalContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethodExtended(final EoParser.HmethodExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodExtended(final EoParser.HmethodExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethodVersioned(final EoParser.HmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodVersioned(final EoParser.HmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethodExtendedVersioned(
        final EoParser.HmethodExtendedVersionedContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitHmethodExtendedVersioned(
        final EoParser.HmethodExtendedVersionedContext ctx
    ) {
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
    public void enterHmethodHeadExtended(final EoParser.HmethodHeadExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodHeadExtended(final EoParser.HmethodHeadExtendedContext ctx) {
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
    public void enterVmethodVersioned(final EoParser.VmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodVersioned(final EoParser.VmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodOptional(final EoParser.VmethodOptionalContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodOptional(final EoParser.VmethodOptionalContext ctx) {
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
    public void enterVmethodHeadCurrent(final EoParser.VmethodHeadCurrentContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodHeadCurrent(final EoParser.VmethodHeadCurrentContext ctx) {
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
    public void enterVmethodHeadApplicationTail(final EoParser.VmethodHeadApplicationTailContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodHeadApplicationTail(final EoParser.VmethodHeadApplicationTailContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodHeadHmethodExtended(final EoParser.VmethodHeadHmethodExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodHeadHmethodExtended(final EoParser.VmethodHeadHmethodExtendedContext ctx) {
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
    public void enterVmethodHeadHapplication(final EoParser.VmethodHeadHapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodHeadHapplication(final EoParser.VmethodHeadHapplicationContext ctx) {
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
    public void enterMethodTailVersioned(final EoParser.MethodTailVersionedContext ctx) {
        this.startObject(ctx)
            .prop("base", String.format(".%s", ctx.NAME().getText()))
            .prop("method");
    }

    @Override
    public void exitMethodTailVersioned(final EoParser.MethodTailVersionedContext ctx) {
        this.objects.leave();
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
        } else if (ctx.AT() != null) {
            base = "@";
        } else if (ctx.RHO() != null) {
            base = "^";
        } else if (ctx.VERTEX() != null) {
            base = "<";
        } else if (ctx.SIGMA() != null) {
            base = "&";
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
    public void enterFinisherCopied(final EoParser.FinisherCopiedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitFinisherCopied(final EoParser.FinisherCopiedContext ctx) {
        if (ctx.COPY() != null) {
            this.objects.enter().prop("copy").leave();
        }
    }

    @Override
    public void enterVersioned(final EoParser.VersionedContext ctx) {
        this.startObject(ctx).prop("base", ctx.NAME().getText());
    }

    @Override
    public void exitVersioned(final EoParser.VersionedContext ctx) {
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
        if (ctx.AT() != null) {
            this.objects.prop("name", ctx.AT().getText());
        } else if (ctx.NAME() != null) {
            this.objects.prop("name", ctx.NAME().getText());
        }
    }

    @Override
    public void exitSuffix(final EoParser.SuffixContext ctx) {
        this.objects.leave();
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
    public void enterVersion(final EoParser.VersionContext ctx) {
        if (ctx.VER() != null) {
            this.objects.prop("ver", ctx.VER().getText());
        }
    }

    @Override
    public void exitVersion(final EoParser.VersionContext ctx) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterAs(final EoParser.AsContext ctx) {
        this.objects.enter();
        final String has;
        if (ctx.NAME() != null) {
            has = ctx.NAME().getText();
        } else if (ctx.INT() != null) {
            has = ctx.INT().getText();
        } else {
            has = "^";
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
        final String type;
        final String data;
        final String base;
        final String text = ctx.getText();
        if (ctx.BYTES() != null) {
            type = "bytes";
            base = "bytes";
            data = text.replaceAll("\\s+", "").replace("-", " ").trim();
        } else if (ctx.BOOL() != null) {
            type = "bytes";
            base = "bool";
            if (Boolean.parseBoolean(text)) {
                data = XeEoListener.bytesToHex((byte) 0x01);
            } else {
                data = XeEoListener.bytesToHex((byte) 0x00);
            }
        } else if (ctx.FLOAT() != null) {
            type = "bytes";
            base = "float";
            data = XeEoListener.bytesToHex(
                ByteBuffer
                    .allocate(Long.BYTES)
                    .putDouble(Double.parseDouble(text))
                    .array()
            );
        } else if (ctx.INT() != null) {
            type = "bytes";
            base = "int";
            data = XeEoListener.bytesToHex(
                ByteBuffer
                    .allocate(Long.BYTES)
                    .putLong(Long.parseLong(text))
                    .array()
            );
        } else if (ctx.HEX() != null) {
            type = "bytes";
            base = "int";
            data = XeEoListener.bytesToHex(
                ByteBuffer
                    .allocate(Long.BYTES)
                    .putLong(Long.parseLong(text.substring(2), 16))
                    .array()
            );
        } else if (ctx.STRING() != null) {
            type = "bytes";
            base = "string";
            data = XeEoListener.bytesToHex(
                StringEscapeUtils.unescapeJava(
                    text.substring(1, text.length() - 1)
                ).getBytes(StandardCharsets.UTF_8)
            );
        } else if (ctx.TEXT() != null) {
            type = "bytes";
            base = "string";
            final int indent = ctx.getStart().getCharPositionInLine();
            data = XeEoListener.bytesToHex(
                StringEscapeUtils.unescapeJava(
                    XeEoListener.trimMargin(text, indent)
                ).getBytes(StandardCharsets.UTF_8)
            );
        } else {
            throw new ParsingException(
                String.format(
                    "Unknown data type at line #%d",
                    ctx.getStart().getLine()
                ),
                new IllegalArgumentException(),
                ctx.getStart().getLine()
            );
        }
        this.objects.prop("data", type);
        this.objects.prop("base", base);
        this.objects.data(data);
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
        return this.startObject(ctx).prop("abstract").leave();
    }

    /**
     * Add error to {@link XeEoListener#errors} directives.
     * @param ctx Context
     * @param check Check type
     * @param severity Error severity level
     * @param message Error message
     */
    private void addError(
        final ParserRuleContext ctx,
        final String check,
        final String severity,
        final String message
    ) {
        this.errors.add("error")
            .attr("line", ctx.getStart().getLine())
            .attr("check", check)
            .attr("severity", severity)
            .set(message)
            .up();
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
     * Bytes to HEX.
     *
     * @param bytes Bytes.
     * @return Hexadecimal value as string.
     */
    private static String bytesToHex(final byte... bytes) {
        final StringJoiner out = new StringJoiner(" ");
        for (final byte bty : bytes) {
            out.add(String.format("%02X", bty));
        }
        return out.toString();
    }
}
