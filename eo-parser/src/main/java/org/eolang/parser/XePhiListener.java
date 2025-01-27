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
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.Stack;
import java.util.function.Supplier;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.text.StringEscapeUtils;
import org.cactoos.list.ListOf;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * The PHI-CALCULUS grammar listener for ANTLR4 walker.
 *
 * @since 0.34.0
 * @checkstyle CyclomaticComplexityCheck (500 lines)
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 * @checkstyle MethodCountCheck (1300 lines)
 * @checkstyle NestedIfDepthCheck (1300 lines)
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.AvoidDuplicateLiterals",
    "PMD.ExcessivePublicCount",
    "PMD.ExcessiveClassLength",
    "PMD.GodClass"
})
final class XePhiListener implements PhiListener, Iterable<Directive> {
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
     * Stack of alphas.
     */
    private final Stack<Integer> alphas;

    /**
     * Objects.
     */
    private final Deque<Objects> objs;

    /**
     * Packages.
     */
    private final Collection<String> packages;

    /**
     * When we start.
     */
    private final long start;

    /**
     * The name of it.
     */
    private final String name;

    /**
     * Ctor.
     * @param nme The name of it
     */
    XePhiListener(final String nme) {
        this.name = nme;
        this.dirs = new Directives();
        this.objs = new ArrayDeque<>();
        this.attributes = new Stack<>();
        this.properties = new Stack<>();
        this.alphas = new Stack<>();
        this.packages = new ListOf<>();
        this.start = System.nanoTime();
    }

    @Override
    public void enterProgram(final PhiParser.ProgramContext ctx) {
        this.objs.add(new Objects.ObjXembly());
        this.dirs
            .append(new DrProgram(this.name))
            .append(new DrListing(ctx))
            .xpath("/program").strict(1);
        if (ctx.object() == null || ctx.object().formation() == null) {
            this.startObject(ctx);
        }
    }

    @Override
    public void exitProgram(final PhiParser.ProgramContext ctx) {
        if (!this.packages.isEmpty()) {
            final String pckg = String.join(".", this.packages);
            this.dirs.addIf("metas").up()
                .xpath("metas[last()]").strict(1)
                .add("meta")
                .attr("line", 1)
                .add("head").set("package").up()
                .add("tail").set(pckg).up()
                .add("part").set(pckg).up()
                .up().up();
        }
        if (ctx.object() == null || ctx.object().formation() == null) {
            this.objects().leave();
        }
        this.dirs.add("objects")
            .append(this.objs.pollLast())
            .up()
            .attr("ms", (System.nanoTime() - this.start) / (1000L * 1000L));
    }

    @Override
    public void enterObject(final PhiParser.ObjectContext ctx) {
        // Nothing here
    }

    @Override
    public void exitObject(final PhiParser.ObjectContext ctx) {
        // Nothing here
    }

    @Override
    public void enterFormation(final PhiParser.FormationContext ctx) {
        this.properties.push("name");
    }

    @Override
    public void exitFormation(final PhiParser.FormationContext ctx) {
        this.properties.pop();
        if (!this.properties.empty() && !XePhiListener.hasLambdaPackage(ctx.bindings())) {
            this.objects().leave();
        }
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterScoped(final PhiParser.ScopedContext ctx) {
        if (ctx.HOME() != null) {
            this.objects().prop("base", "Q");
        } else if (ctx.DEF_PACKAGE() != null) {
            final int line = ctx.getStart().getLine();
            final int pos = ctx.getStart().getCharPositionInLine();
            this.objects().prop("base", "Q").leave()
                .start(line, pos + 1).prop("base", ".org").prop("method").leave()
                .start(line, pos + 5).prop("base", ".eolang").prop("method");
        } else {
            this.objects().prop("base", "$");
        }
    }

    @Override
    public void exitScoped(final PhiParser.ScopedContext ctx) {
        this.objects().leave();
    }

    @Override
    public void enterBindings(final PhiParser.BindingsContext ctx) {
        if (XePhiListener.hasLambdaPackage(ctx)) {
            this.packages.add(this.attributes.peek());
            this.objs.add(new Objects.ObjXembly());
        }
    }

    @Override
    public void exitBindings(final PhiParser.BindingsContext ctx) {
        if (XePhiListener.hasLambdaPackage(ctx)) {
            this.objs.poll();
        }
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterBinding(final PhiParser.BindingContext ctx) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void exitBinding(final PhiParser.BindingContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTauBinding(final PhiParser.TauBindingContext ctx) {
        this.startObject(ctx);
    }

    @Override
    public void exitTauBinding(final PhiParser.TauBindingContext ctx) {
        this.exitObjectBinding();
    }

    @Override
    public void enterApplicationTauBinding(final PhiParser.ApplicationTauBindingContext ctx) {
        this.startObject(ctx);
    }

    @Override
    public void exitApplicationTauBinding(final PhiParser.ApplicationTauBindingContext ctx) {
        this.exitObjectBinding();
    }

    @Override
    public void enterVoids(final PhiParser.VoidsContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVoids(final PhiParser.VoidsContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVoid(final PhiParser.VoidContext ctx) {
        final String nme;
        if (ctx.PHI() != null) {
            nme = "@";
        } else if (ctx.LABEL() != null || ctx.ALPHA() != null) {
            nme = ctx.getText();
        } else  {
            nme = "";
        }
        this.addVoidAttribute(ctx).prop("name", nme);
    }

    @Override
    public void exitVoid(final PhiParser.VoidContext ctx) {
        this.objects().leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterAttribute(final PhiParser.AttributeContext ctx) {
        final String attr;
        if (ctx.PHI() != null) {
            attr = "@";
        } else if (ctx.RHO() != null) {
            attr = "^";
        } else if (ctx.LABEL() != null) {
            attr = ctx.getText();
        } else {
            attr = "";
        }
        this.attributes.push(attr);
    }

    @Override
    public void exitAttribute(final PhiParser.AttributeContext ctx) {
        // Nothing here
    }

    @Override
    public void enterFullAttribute(final PhiParser.FullAttributeContext ctx) {
        if (ctx.attribute() == null) {
            this.attributes.push(ctx.getText());
        }
    }

    @Override
    public void exitFullAttribute(final PhiParser.FullAttributeContext ctx) {
        // Nothing here
    }

    @Override
    public void enterEmptyBinding(final PhiParser.EmptyBindingContext ctx) {
        this.addVoidAttribute(ctx);
    }

    @Override
    public void exitEmptyBinding(final PhiParser.EmptyBindingContext ctx) {
        this.objects().leave();
        this.exitObjectBinding();
    }

    @Override
    public void enterDeltaBinding(final PhiParser.DeltaBindingContext ctx) {
        if (ctx.EMPTY() != null) {
            if (!"org.eolang".equals(String.join(".", this.packages))
                && !"bytes".equals(this.attributes.peek())
            ) {
                throw new ParsingException(
                    new IllegalStateException(), ctx.getStart().getLine(),
                    "It's impossible to represent Δ ⤍ ∅ binding in EO"
                );
            }
        } else {
            this.objects().data(ctx.BYTES().getText().trim());
        }
    }

    @Override
    public void exitDeltaBinding(final PhiParser.DeltaBindingContext ctx) {
        // Nothing here
    }

    @Override
    public void enterLambdaBinding(final PhiParser.LambdaBindingContext ctx) {
        if (!XePhiListener.LAMBDA_PACKAGE.equals(ctx.FUNCTION().getText())) {
            this.objects().prop("atom", ctx.FUNCTION().getText());
        }
    }

    @Override
    public void exitLambdaBinding(final PhiParser.LambdaBindingContext ctx) {
        // Nothing here
    }

    @Override
    public void enterApplication(final PhiParser.ApplicationContext ctx) {
        this.properties.push("as");
        this.objects().enter();
    }

    @Override
    public void exitApplication(final PhiParser.ApplicationContext ctx) {
        this.objects().leave();
        this.properties.pop();
    }

    @Override
    public void enterApplicationBindings(final PhiParser.ApplicationBindingsContext ctx) {
        // Nothing here
    }

    @Override
    public void exitApplicationBindings(final PhiParser.ApplicationBindingsContext ctx) {
        // Nothing here
    }

    @Override
    public void enterApplicationObjects(final PhiParser.ApplicationObjectsContext ctx) {
        this.alphas.push(0);
    }

    @Override
    public void exitApplicationObjects(final PhiParser.ApplicationObjectsContext ctx) {
        this.alphas.pop();
    }

    @Override
    public void enterJustObject(final PhiParser.JustObjectContext ctx) {
        this.startObject(ctx);
        final int index = this.alphas.peek();
        this.alphas.pop();
        this.alphas.push(index + 1);
        this.attributes.push(String.format("α%d", index));
    }

    @Override
    public void exitJustObject(final PhiParser.JustObjectContext ctx) {
        this.exitObjectBinding();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterDispatch(final PhiParser.DispatchContext ctx) {
        this.startObject(ctx).prop("method");
    }

    @Override
    public void exitDispatch(final PhiParser.DispatchContext ctx) {
        final String attr = this.attributes.pop();
        if (!attr.isEmpty()) {
            this.objects().prop("base", String.format(".%s", attr));
        }
        this.objects().leave();
    }

    @Override
    public void enterApplicationsOrDispatches(final PhiParser.ApplicationsOrDispatchesContext ctx) {
        // Nothing here
    }

    @Override
    public void exitApplicationsOrDispatches(final PhiParser.ApplicationsOrDispatchesContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTermination(final PhiParser.TerminationContext ctx) {
        this.objects().prop("base", "org.eolang.error").leave();
    }

    @Override
    public void exitTermination(final PhiParser.TerminationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterData(final PhiParser.DataContext ctx) {
        final String base;
        final Supplier<String> data;
        final String text = ctx.getText();
        if (ctx.FLOAT() != null || ctx.INT() != null) {
            base = "org.eolang.number";
            data = new BytesToHex(
                ByteBuffer
                    .allocate(Double.BYTES)
                    .putDouble(Double.parseDouble(text))
                    .array()
            );
        } else {
            base = "org.eolang.string";
            data = new BytesToHex(
                StringEscapeUtils.unescapeJava(
                    text.substring(1, text.length() - 1)
                ).getBytes(StandardCharsets.UTF_8)
            );
        }
        this.objects()
            .prop("base", base)
            .start(
                ctx.getStart().getLine(),
                ctx.getStart().getCharPositionInLine() + base.length() + 1
            )
            .prop("base", "org.eolang.bytes")
            .data(data.get())
            .leave();
    }

    @Override
    public void exitData(final PhiParser.DataContext ctx) {
        this.objects().leave();
    }

    @Override
    public void visitTerminal(final TerminalNode node) {
        // Nothing here
    }

    @Override
    public void visitErrorNode(final ErrorNode node) {
        // Nothing here
    }

    @Override
    public void enterEveryRule(final ParserRuleContext ctx) {
        // Nothing here
    }

    @Override
    public void exitEveryRule(final ParserRuleContext ctx) {
        // Nothing here
    }

    @Override
    public Iterator<Directive> iterator() {
        return this.dirs.iterator();
    }

    /**
     * Objects at the last level of stack.
     * @return Objects
     */
    private Objects objects() {
        return this.objs.getLast();
    }

    /**
     * Adds void attribute to the current object.
     * @param ctx Context
     * @return Objects
     */
    private Objects addVoidAttribute(final ParserRuleContext ctx) {
        return this.startObject(ctx).prop("base", "∅");
    }

    /**
     * Start object.
     * @param ctx Parser rule context.
     * @return Objects
     */
    private Objects startObject(final ParserRuleContext ctx) {
        return this.objects().start(
            ctx.getStart().getLine(), ctx.getStart().getCharPositionInLine()
        );
    }

    /**
     * Exit either tau or empty binding.
     */
    private void exitObjectBinding() {
        if (this.objs.size() > this.packages.size()) {
            this.objects().enter()
                .prop(this.properties.peek(), this.attributes.pop())
                .leave();
        }
    }

    /**
     * Check if bindings on the given context have lambda package.
     * @param ctx Context
     * @return If bindings have lambda package
     */
    private static boolean hasLambdaPackage(final PhiParser.BindingsContext ctx) {
        return ctx.binding()
            .stream()
            .anyMatch(
                context -> context.lambdaBinding() != null && context.lambdaBinding()
                    .FUNCTION()
                    .getText()
                    .equals(XePhiListener.LAMBDA_PACKAGE)
            );
    }
}
