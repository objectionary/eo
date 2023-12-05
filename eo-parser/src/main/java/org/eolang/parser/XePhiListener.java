/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.cactoos.list.ListOf;
import org.xembly.Directive;
import org.xembly.Directives;

public final class XePhiListener implements PhiListener, Iterable<Directive> {
    /**
     * Package lambda.
     */
    private static final String LAMBDA_PACKAGE = "Package";

    /**
     * Xembly directives we are building (mutable).
     */
    private final Directives dirs;

    /**
     * Attributes stack.
     */
    private final Stack<String> attributes;

    /**
     * XMIR properties.
     */
    private final Stack<String> properties;

    /**
     * Objects.
     */
    private final Deque<Objects> objects;

    /**
     * Package.
     */
    private final Collection<String> pckg;

    /**
     * When we start.
     */
    private long start;

    /**
     * Ctor.
     */
    public XePhiListener() {
        this.dirs = new Directives();
        this.objects = new ArrayDeque<>();
        this.attributes = new Stack<>();
        this.properties = new Stack<>();
        this.pckg = new ListOf<>();
    }

    @Override
    public void enterProgram(final PhiParser.ProgramContext ctx) {
        this.start = System.nanoTime();
        this.objects.add(new Objects.ObjXembly());
        this.dirs.add("program")
//            .attr("version", Manifests.read("EO-Version"))
//            .attr("revision", Manifests.read("EO-Revision"))
//            .attr("dob", Manifests.read("EO-Dob"))
            .attr(
                "time",
                ZonedDateTime.now(ZoneOffset.UTC).format(DateTimeFormatter.ISO_INSTANT)
            )
            .add("listing").set(XePhiListener.sourceText(ctx)).up();
        this.properties.push("name");
    }

    @Override
    public void exitProgram(final PhiParser.ProgramContext ctx) {
        this.properties.pop();
        if (!this.pckg.isEmpty()) {
            final String tail = String.join(".", this.pckg);
            this.dirs.add("metas")
                .add("meta")
                .add("head").set("package").up()
                .add("tail").set(tail).up()
                .add("part").set(tail).up()
                .up().up();
        }
        this.dirs.add("objects").append(this.objects.pollLast());
    }

    @Override
    public void enterObject(final PhiParser.ObjectContext ctx) {
    }

    @Override
    public void exitObject(final PhiParser.ObjectContext ctx) {
    }

    @Override
    public void enterFormation(final PhiParser.FormationContext ctx) {
        this.properties.push("name");
    }

    @Override
    public void exitFormation(final PhiParser.FormationContext ctx) {
        this.properties.pop();
        if (!XePhiListener.hasLambdaPackage(ctx.bindings())) {
            final String pop = this.attributes.pop();
            System.out.printf("pop %s\n", pop);
            this.objects()
                .prop("abstract")
                .prop("name", pop);
        }
    }

    @Override
    public void enterBindings(final PhiParser.BindingsContext ctx) {
        if (XePhiListener.hasLambdaPackage(ctx)) {
            this.pckg.add(this.attributes.peek());
            this.objects.add(new Objects.ObjXembly());
        }
    }

    @Override
    public void exitBindings(final PhiParser.BindingsContext ctx) {
        if (XePhiListener.hasLambdaPackage(ctx)) {
            this.objects.poll();
        }
    }

    @Override
    public void enterBinding(final PhiParser.BindingContext ctx) {
        if (ctx.alphaBinding() != null || ctx.emptyBinding() != null) {
            this.objects().start();
        }
    }

    @Override
    public void exitBinding(final PhiParser.BindingContext ctx) {
        if (ctx.alphaBinding() != null || ctx.emptyBinding() != null) {
            if (this.objects.size() > this.pckg.size()) {
                this.objects().leave();
            }
        }
    }

    @Override
    public void enterAlphaBinding(final PhiParser.AlphaBindingContext ctx) {
    }

    @Override
    public void exitAlphaBinding(final PhiParser.AlphaBindingContext ctx) {

    }

    @Override
    public void enterAttribute(final PhiParser.AttributeContext ctx) {
        final String attr;
        if (ctx.PHI() != null) {
            attr = "@";
        } else if (ctx.RHO() != null) {
            attr = "^";
        } else if (ctx.SIGMA() != null) {
            attr = "&";
        } else if (ctx.VTX() != null) {
            attr = "<";
        } else if (ctx.LABEL() != null) {
            attr = ctx.LABEL().getText();
        } else if (ctx.alpha() != null) {
            attr = ctx.alpha().INDEX().getText();
        } else {
            attr = "";
        }
        this.attributes.push(attr);
        System.out.printf("push %s\n", attr);
    }

    @Override
    public void exitAttribute(final PhiParser.AttributeContext ctx) {

    }

    @Override
    public void enterAlpha(final PhiParser.AlphaContext ctx) {

    }

    @Override
    public void exitAlpha(final PhiParser.AlphaContext ctx) {

    }

    @Override
    public void enterEmptyBinding(final PhiParser.EmptyBindingContext ctx) {
    }

    @Override
    public void exitEmptyBinding(final PhiParser.EmptyBindingContext ctx) {
        final String pop = this.attributes.pop();
        System.out.printf("pop %s\n", pop);
        this.objects().prop("name", pop);
    }

    @Override
    public void enterDeltaBidning(final PhiParser.DeltaBidningContext ctx) {
        if (!ctx.BYTES().getText().equals("--")) {
            this.objects().data(
                ctx.BYTES().getText()
                    .replaceAll("-", " ")
                    .trim()
            );
        }
    }

    @Override
    public void exitDeltaBidning(final PhiParser.DeltaBidningContext ctx) {

    }

    @Override
    public void enterLambdaBidning(final PhiParser.LambdaBidningContext ctx) {
        if (!ctx.FUNCTION().getText().equals(XePhiListener.LAMBDA_PACKAGE)) {
            this.objects().prop("atom");
        }
    }

    @Override
    public void exitLambdaBidning(final PhiParser.LambdaBidningContext ctx) {

    }

    @Override
    public void enterApplication(final PhiParser.ApplicationContext ctx) {

    }

    @Override
    public void exitApplication(final PhiParser.ApplicationContext ctx) {

    }

    @Override
    public void enterBnds(final PhiParser.BndsContext ctx) {
        this.properties.push("as");
    }

    @Override
    public void exitBnds(final PhiParser.BndsContext ctx) {
        this.properties.pop();
    }

    @Override
    public void enterDispatch(final PhiParser.DispatchContext ctx) {
        if (ctx.HOME() != null) {
            this.objects().prop("base", "Q").leave();
        } else if (ctx.XI() != null) {
            this.objects().prop("base", "$").leave();
        }
    }

    @Override
    public void exitDispatch(final PhiParser.DispatchContext ctx) {
        this.objects().enter();
        final String pop = this.attributes.pop();
        System.out.printf("poped %s\n", pop);
        if (!pop.isEmpty()) {
            this.objects().prop(this.properties.peek(), pop);
        }
    }

    @Override
    public void enterDisp(PhiParser.DispContext ctx) {
    }

    @Override
    public void exitDisp(PhiParser.DispContext ctx) {
    }

    @Override
    public void enterDispBnds(PhiParser.DispBndsContext ctx) {
        this.objects().enter();
    }

    @Override
    public void exitDispBnds(PhiParser.DispBndsContext ctx) {
        this.objects().leave();
    }

    @Override
    public void enterAttr(final PhiParser.AttrContext ctx) {
        this.objects().start();
    }

    @Override
    public void exitAttr(final PhiParser.AttrContext ctx) {
        final String pop = this.attributes.pop();
        System.out.printf("poped %s\n", pop);
        this.objects()
            .prop("method")
            .prop("base", String.format(".%s", pop))
            .leave();
    }

    @Override
    public void enterTermination(final PhiParser.TerminationContext ctx) {

    }

    @Override
    public void exitTermination(final PhiParser.TerminationContext ctx) {

    }

    @Override
    public void visitTerminal(TerminalNode terminalNode) {

    }

    @Override
    public void visitErrorNode(ErrorNode errorNode) {

    }

    @Override
    public void enterEveryRule(ParserRuleContext parserRuleContext) {

    }

    @Override
    public void exitEveryRule(ParserRuleContext parserRuleContext) {

    }

    @Override
    public Iterator<Directive> iterator() {
        return this.dirs.iterator();
    }

    private Objects objects() {
        return this.objects.getLast();
    }

    private static boolean hasLambdaPackage(final PhiParser.BindingsContext ctx) {
        return ctx.binding()
            .stream()
            .anyMatch(context -> context.lambdaBidning() != null
                && context.lambdaBidning().FUNCTION().getText().equals(XePhiListener.LAMBDA_PACKAGE));
    }

//    /**
//     * Start object.
//     * @param ctx Context.
//     * @return Started object.
//     */
//    private Objects startObject(final ParserRuleContext ctx) {
//        return this.objects.start(
//            ctx.getStart().getLine(),
//            ctx.getStart().getCharPositionInLine()
//        );
//    }

    /**
     * Text source code.
     * @param ctx Program context.
     * @return Original code.
     */
    private static String sourceText(final PhiParser.ProgramContext ctx) {
        return ctx.getStart().getInputStream().getText(
            new Interval(
                ctx.getStart().getStartIndex(),
                ctx.getStop().getStopIndex()
            )
        );
    }
}