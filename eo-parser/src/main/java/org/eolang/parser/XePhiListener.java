/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
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
     * Alpha.
     */
    private static final String ALPHA = "α";

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
     * Errors.
     */
    private final List<ParsingException> errors;

    /**
     * Attribute names.
     */
    private final Deque<Set<String>> anames;

    /**
     * Ctor.
     */
    XePhiListener() {
        this.dirs = new Directives();
        this.objs = new ArrayDeque<>();
        this.attributes = new Stack<>();
        this.properties = new Stack<>();
        this.alphas = new Stack<>();
        this.packages = new ListOf<>();
        this.errors = new ArrayList<>(0);
        this.start = System.nanoTime();
        this.anames = new ArrayDeque<>();
    }

    @Override
    public void enterProgram(final PhiParser.ProgramContext ctx) {
        this.objs.add(new Objects());
        this.dirs
            .append(new DrProgram())
            .append(new DrListing(ctx))
            .xpath("/object").strict(1);
        if (ctx.object() == null || ctx.object().formation() == null) {
            this.objects().start(ctx);
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
        if (!this.errors.isEmpty()) {
            this.dirs.append(new DrErrors(this.errors));
        }
        this.dirs
            .append(this.objs.pollLast())
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
        } else if (ctx.XI() != null) {
            this.objects().prop("base", "$");
        } else {
            this.objects().prop("base", "$")
                .leave()
                .start(ctx.getStart().getLine(), ctx.getStart().getCharPositionInLine() + 1)
                .prop("method");
        }
    }

    @Override
    public void exitScoped(final PhiParser.ScopedContext ctx) {
        if (ctx.fullAttribute() != null) {
            this.objects().prop("base", String.format(".%s", this.attributes.pop()));
        }
        this.objects().leave();
    }

    @Override
    public void enterBindings(final PhiParser.BindingsContext ctx) {
        this.anames.push(new HashSet<>());
        if (XePhiListener.hasLambdaPackage(ctx)) {
            this.packages.add(this.attributes.peek());
            this.objs.add(new Objects());
        }
    }

    @Override
    public void exitBindings(final PhiParser.BindingsContext ctx) {
        if (XePhiListener.hasLambdaPackage(ctx)) {
            this.objs.poll();
        }
        this.anames.pop();
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
        this.objects().start(ctx);
        final PhiParser.AttributeContext cattr = ctx.attribute();
        this.checkDuplicates(cattr, cattr.getText());
    }

    @Override
    public void exitTauBinding(final PhiParser.TauBindingContext ctx) {
        this.exitObjectBinding();
    }

    @Override
    public void enterApplicationTauBinding(final PhiParser.ApplicationTauBindingContext ctx) {
        this.objects().start(ctx);
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
        } else if (ctx.LABEL() != null) {
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
            this.attributes.push(
                String.format("%s%s", XePhiListener.ALPHA, ctx.getText().substring(1))
            );
        }
    }

    @Override
    public void exitFullAttribute(final PhiParser.FullAttributeContext ctx) {
        // Nothing here
    }

    @Override
    public void enterEmptyBinding(final PhiParser.EmptyBindingContext ctx) {
        this.addVoidAttribute(ctx);
        this.checkDuplicates(ctx, ctx.attribute().getText());
    }

    @Override
    public void exitEmptyBinding(final PhiParser.EmptyBindingContext ctx) {
        this.objects().leave();
        this.exitObjectBinding();
    }

    @Override
    public void enterDeltaBinding(final PhiParser.DeltaBindingContext ctx) {
        if (ctx.EMPTY() != null) {
            this.errors.add(
                new ParsingError(
                    ctx, "It's impossible to represent Δ ⤍ ∅ binding in EO"
                ).cause()
            );
        } else {
            this.objects().data(ctx.BYTES().getText().trim());
            this.checkDuplicates(ctx, "Δ");
        }
    }

    @Override
    public void exitDeltaBinding(final PhiParser.DeltaBindingContext ctx) {
        // Nothing here
    }

    @Override
    public void enterLambdaBinding(final PhiParser.LambdaBindingContext ctx) {
        if (!XePhiListener.LAMBDA_PACKAGE.equals(ctx.FUNCTION().getText())) {
            this.objects().start(ctx).prop("name", "λ").leave();
            this.checkDuplicates(ctx, "λ");
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
        this.objects().start(ctx);
        final int index = this.alphas.peek();
        this.alphas.pop();
        this.alphas.push(index + 1);
        this.attributes.push(String.format("%s%d", XePhiListener.ALPHA, index));
    }

    @Override
    public void exitJustObject(final PhiParser.JustObjectContext ctx) {
        this.exitObjectBinding();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterDispatch(final PhiParser.DispatchContext ctx) {
        this.objects().start(ctx).prop("method");
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
        this.objects().prop("base", "Q.org.eolang.error").leave();
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
            base = "Q.org.eolang.number";
            data = new BytesToHex(
                ByteBuffer
                    .allocate(Double.BYTES)
                    .putDouble(Double.parseDouble(text))
                    .array()
            );
        } else {
            base = "Q.org.eolang.string";
            data = new BytesToHex(
                StringEscapeUtils.unescapeJava(
                    text.substring(1, text.length() - 1)
                ).getBytes(StandardCharsets.UTF_8)
            );
        }
        final int line = ctx.getStart().getLine();
        final int pos = ctx.getStart().getCharPositionInLine() + base.length() + 1;
        this.objects()
            .prop("base", base)
            .start(line, pos)
            .prop("base", "Q.org.eolang.bytes")
            .start(line, pos)
            .data(data.get())
            .leave()
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
        return this.objects().start(ctx).prop("base", "∅");
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

    /**
     * Check the duplicate attributes.
     * @param ctx Parsing context
     * @param aname Attribute name
     */
    // it does not understand scopes correctly
    private void checkDuplicates(final ParserRuleContext ctx, final String aname) {
        final Set<String> cscope = this.anames.peek();
        if (cscope != null) {
            if (cscope.contains(aname) && !"^".equals(aname)) {
                this.errors.add(
                    new ParsingError(
                        ctx, String.format("Attribute '%s' is duplicated", aname)
                    ).cause()
                );
            } else {
                cscope.add(aname);
            }
        }
    }
}
