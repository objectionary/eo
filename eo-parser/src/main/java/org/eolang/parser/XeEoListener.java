/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.text.StringEscapeUtils;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * The EO grammar listener for ANTLR4 walker.
 *
 * @since 0.1
 * @checkstyle CyclomaticComplexityCheck (500 lines)
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 * @checkstyle MethodCountCheck (1300 lines)
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.AvoidDuplicateLiterals",
    "PMD.ExcessivePublicCount",
    "PMD.CouplingBetweenObjects"
})
final class XeEoListener extends EoBaseListener implements Iterable<Directive> {
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
    public void enterMetas(final EoParser.MetasContext ctx) {
        this.dirs.addIf("metas");
        for (final TerminalNode node : ctx.META()) {
            final String[] pair = node.getText().split(" ", 2);
            this.dirs.add("meta")
                .attr("line", node.getSymbol().getLine())
                .add("head").set(pair[0].substring(1)).up()
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
    public void enterCommentOptional(final EoParser.CommentOptionalContext ctx) {
        this.putComment(ctx.comment(), ctx.getStop());
    }

    @Override
    public void enterAtom(final EoParser.AtomContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitAtom(final EoParser.AtomContext ctx) {
        this.objects.enter()
            .start(ctx.getStart().getLine(), 0)
            .prop("name", "λ")
            .leave()
            .leave();
    }

    @Override
    public void enterFormation(final EoParser.FormationContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void enterTformation(final EoParser.TformationContext ctx) {
        this.startTest(ctx);
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
            name = "φ";
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
    public void enterHapplicationArgScoped(final EoParser.HapplicationArgScopedContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void exitCompactTuple(final EoParser.CompactTupleContext ctx) {
        final int count;
        if (ctx.INT() != null) {
            final String num = ctx.INT().getText();
            final int number = Integer.parseInt(num);
            if (XeEoListener.invalidIndex(num, number)) {
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
        this.objects.enter().prop("before-star", count).leave();
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
    public void enterVapplicationArgBoundNext(final EoParser.VapplicationArgBoundNextContext ctx) {
        if (ctx.commentOptional() != null && ctx.voids() != null) {
            this.startAbstract(ctx);
        }
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
    public void enterFormationNamed(final EoParser.FormationNamedContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void enterHformation(final EoParser.HformationContext ctx) {
        this.startAbstract(ctx);
    }

    @Override
    public void enterOnlyphi(final EoParser.OnlyphiContext ctx) {
        this.startAbstract(ctx).enter();
    }

    @Override
    public void enterOnlyphiTail(final EoParser.OnlyphiTailContext ctx) {
        this.objects.enter().prop("name", "φ").leave().leave();
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
    public void exitMethodTail(final EoParser.MethodTailContext ctx) {
        this.objects
            .enter()
            .prop("method")
            .xprop("base", "concat('.',@base)")
            .xprop("pos", "@pos - 1")
            .leave();
    }

    @Override
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
    public void exitOname(final EoParser.OnameContext ctx) {
        if (ctx.CONST() != null) {
            this.objects.enter().prop("const").leave();
        }
    }

    @Override
    public void enterTname(final EoParser.TnameContext ctx) {
        this.objects.enter();
        if (ctx.PHI() != null) {
            this.objects.prop("name", "φ");
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
    public void enterSuffix(final EoParser.SuffixContext ctx) {
        this.objects.enter();
        if (ctx.PHI() != null) {
            this.objects.prop("name", "φ");
        } else if (ctx.NAME() != null) {
            this.objects.prop("name", ctx.NAME().getText());
        }
    }

    @Override
    public void exitSuffix(final EoParser.SuffixContext ctx) {
        this.objects.leave();
    }

    @Override
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
                        .putDouble((double) Long.parseLong(text.substring(2), 16))
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
                data = new BytesToHex(
                    StringEscapeUtils.unescapeJava(
                        XeEoListener.trimMargin(
                            text, ctx.getStart().getCharPositionInLine()
                        )
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
        return this.objects.start(ctx)
            .start(ctx)
            .prop("base", "ξ")
            .prop("name", "xi🌵")
            .leave()
            .leave();
    }

    private Objects startTest(final ParserRuleContext ctx) {
        return this.objects.start(ctx).leave();
    }

    /**
     * Build comment from context.
     *
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
                            result = context.COMMENTARY().getText().substring(
                                XeEoListener.commentOffset(text)
                            );
                        }
                        return result;
                    }
                ).collect(Collectors.joining("\\n"))
            ).attr("line", stop.getLine() + 1).pop();
        }
    }

    /**
     * Extract positive integer from context and convert to alpha attribute.
     *
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
     * Trim margin from text block.
     *
     * @param text   Text block.
     * @param indent Indentation level.
     * @return Trimmed text.
     */
    private static String trimMargin(final String text, final int indent) {
        final String cutted = text
            .substring(3, text.length() - 3).trim();
        final String[] splitted = cutted.split("\n");
        StringBuilder res = new StringBuilder();
        for (final String line : splitted) {
            res.append(
                line.replaceAll(String.format("[\\s]{%d}", indent), "")
            ).append('\n');
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
     * Calculate comment offset based on whether text starts with space.
     *
     * @param text Comment text
     * @return Offset to use for substring
     */
    private static int commentOffset(final String text) {
        final int offset;
        if (text.startsWith(" ", 1)) {
            offset = 2;
        } else {
            offset = 1;
        }
        return offset;
    }

    /**
     * Check if compact tuple index is invalid.
     *
     * @param num Index string representation
     * @param number Parsed index value
     * @return True if invalid
     */
    private static boolean invalidIndex(final String num, final int number) {
        final char first = num.charAt(0);
        return first == '+' || first == '-'
            || !"0".equals(num) && first == '0'
            || number < 0;
    }

    /**
     * Translate FQN starting with `Q` or `QQ` to the one starting with a global Phi object.
     *
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
