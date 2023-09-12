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

import com.jcabi.manifests.Manifests;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Iterator;
import java.util.StringJoiner;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.text.StringEscapeUtils;
import org.cactoos.iterable.Mapped;
import org.cactoos.text.Joined;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * The listener for ANTLR4 walker.
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
    "PMD.ExcessiveClassLength"
})
public final class XeListener implements ProgramListener, Iterable<Directive> {

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
     * Ctor.
     *
     * @param name The name of it
     */
    public XeListener(final String name) {
        this.name = name;
        this.dirs = new Directives();
        this.objects = new Objects.ObjXembly();
        this.start = System.nanoTime();
    }

    @Override
    public void enterProgram(final ProgramParser.ProgramContext ctx) {
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
            .add("listing").set(XeListener.sourceText(ctx)).up()
            .add("errors").up()
            .add("sheets").up()
            .add("license").up()
            .add("metas").up();
    }

    @Override
    public void exitProgram(final ProgramParser.ProgramContext ctx) {
        this.dirs
            .attr("ms", (System.nanoTime() - this.start) / (1000L * 1000L))
            .up();
    }

    @Override
    public void enterLicense(final ProgramParser.LicenseContext ctx) {
        this.dirs.addIf("license").set(
            new Joined(
                "\n",
                new Mapped<>(
                    cmt -> cmt.getText().substring(1).trim(),
                    ctx.COMMENT()
                )
            )
        ).up();
    }

    @Override
    public void exitLicense(final ProgramParser.LicenseContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMetas(final ProgramParser.MetasContext ctx) {
        this.dirs.addIf("metas");
        for (final TerminalNode node : ctx.META()) {
            final String[] pair = node.getText().split(" ", 2);
            this.dirs.add("meta")
                .attr("line", node.getSymbol().getLine())
                .add("head").set(pair[0].substring(1)).up()
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
    public void exitMetas(final ProgramParser.MetasContext ctx) {
        // Nothing here
    }

    @Override
    public void enterObjects(final ProgramParser.ObjectsContext ctx) {
        this.dirs.add("objects");
    }

    @Override
    public void exitObjects(final ProgramParser.ObjectsContext ctx) {
        this.dirs.append(this.objects);
        this.dirs.up();
    }

    @Override
    public void enterObject(final ProgramParser.ObjectContext ctx) {
        // Nothing here
    }

    @Override
    public void exitObject(final ProgramParser.ObjectContext ctx) {
        // Nothing here
    }

    @Override
    public void enterJust(final ProgramParser.JustContext ctx) {
        // Nothing here
    }

    @Override
    public void exitJust(final ProgramParser.JustContext ctx) {
        // Nothing here
    }

    @Override
    public void enterJustNamed(final ProgramParser.JustNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitJustNamed(final ProgramParser.JustNamedContext ctx) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterAtom(final ProgramParser.AtomContext ctx) {
        this.startObject(ctx);
        if (ctx.type().NAME() != null) {
            this.objects.prop("atom", ctx.type().NAME().getText());
        } else if (ctx.type().QUESTION() != null) {
            this.objects.prop("atom", ctx.type().QUESTION().getText());
        }
        this.objects.leave();
    }

    @Override
    public void exitAtom(final ProgramParser.AtomContext ctx) {
        // Nothing here
    }

    @Override
    public void enterAbstraction(final ProgramParser.AbstractionContext ctx) {
        this.startObject(ctx);
        this.objects.prop("abstract");
        this.objects.leave();
    }

    @Override
    public void exitAbstraction(final ProgramParser.AbstractionContext ctx) {
        // Nothing here
    }

    @Override
    public void enterInners(final ProgramParser.InnersContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitInners(final ProgramParser.InnersContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterAttributes(final ProgramParser.AttributesContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitAttributes(final ProgramParser.AttributesContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterAttribute(final ProgramParser.AttributeContext ctx) {
        this.startObject(ctx);
        this.objects.prop("name", ctx.NAME().getText());
    }

    @Override
    public void exitAttribute(final ProgramParser.AttributeContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterType(final ProgramParser.TypeContext ctx) {
        // Nothing here
    }

    @Override
    public void exitType(final ProgramParser.TypeContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVararg(final ProgramParser.VarargContext ctx) {
        this.startObject(ctx);
        this.objects.prop("vararg");
        this.objects.prop("name", ctx.NAME().getText());
    }

    @Override
    public void exitVararg(final ProgramParser.VarargContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterApplication(final ProgramParser.ApplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitApplication(final ProgramParser.ApplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationExtended(final ProgramParser.HapplicationExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationExtended(final ProgramParser.HapplicationExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplication(final ProgramParser.HapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplication(final ProgramParser.HapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationHead(final ProgramParser.HapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationHead(final ProgramParser.HapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationHeadExtended(
        final ProgramParser.HapplicationHeadExtendedContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitHapplicationHeadExtended(
        final ProgramParser.HapplicationHeadExtendedContext ctx
    ) {
        // Nothing here
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterApplicable(final ProgramParser.ApplicableContext ctx) {
        if (ctx.reversed() == null) {
            this.startObject(ctx);
            final String base;
            if (ctx.STAR() != null) {
                base = "tuple";
                this.objects.prop("data", "tuple");
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
    }

    @Override
    public void exitApplicable(final ProgramParser.ApplicableContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHapplicationTail(final ProgramParser.HapplicationTailContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationTail(final ProgramParser.HapplicationTailContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterHapplicationArg(final ProgramParser.HapplicationArgContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHapplicationArg(final ProgramParser.HapplicationArgContext ctx) {
        if (ctx.DOTS() != null) {
            this.objects.enter();
            this.objects.prop("unvar");
            this.objects.leave();
        }
    }

    @Override
    public void enterHapplicationTailExtended(
        final ProgramParser.HapplicationTailExtendedContext ctx
    ) {
        this.objects.enter();
    }

    @Override
    public void exitHapplicationTailExtended(
        final ProgramParser.HapplicationTailExtendedContext ctx
    ) {
        this.objects.leave();
    }

    @Override
    public void enterHapplicationArgExtended(
        final ProgramParser.HapplicationArgExtendedContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitHapplicationArgExtended(
        final ProgramParser.HapplicationArgExtendedContext ctx
    ) {
        if (ctx.DOTS() != null) {
            this.objects.enter();
            this.objects.prop("unvar");
            this.objects.leave();
        }
    }

    @Override
    public void enterVapplication(final ProgramParser.VapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplication(final ProgramParser.VapplicationContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationHead(final ProgramParser.VapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationHead(final ProgramParser.VapplicationHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationHeadNamed(final ProgramParser.VapplicationHeadNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationHeadNamed(final ProgramParser.VapplicationHeadNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgs(final ProgramParser.VapplicationArgsContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitVapplicationArgs(final ProgramParser.VapplicationArgsContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterVapplicationArgSpreadable(
        final ProgramParser.VapplicationArgSpreadableContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgSpreadable(
        final ProgramParser.VapplicationArgSpreadableContext ctx
    ) {
        if (ctx.DOTS() != null) {
            this.objects.enter();
            this.objects.prop("unvar");
            this.objects.leave();
        }
    }

    @Override
    public void enterVapplicationArgHapplication(
        final ProgramParser.VapplicationArgHapplicationContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHapplication(
        final ProgramParser.VapplicationArgHapplicationContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgVapplication(
        final ProgramParser.VapplicationArgVapplicationContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgVapplication(
        final ProgramParser.VapplicationArgVapplicationContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterVapplicationHeadAs(
        final ProgramParser.VapplicationHeadAsContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitVapplicationHeadAs(final ProgramParser.VapplicationHeadAsContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgVanonym(final ProgramParser.VapplicationArgVanonymContext ctx) {
        this.startObject(ctx);
        this.objects.prop("abstract");
        this.objects.leave();
    }

    @Override
    public void exitVapplicationArgVanonym(final ProgramParser.VapplicationArgVanonymContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVapplicationArgHanonym(final ProgramParser.VapplicationArgHanonymContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVapplicationArgHanonym(final ProgramParser.VapplicationArgHanonymContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHanonym(final ProgramParser.HanonymContext ctx) {
        this.startObject(ctx);
        this.objects.prop("abstract");
        this.objects.leave();
    }

    @Override
    public void exitHanonym(final ProgramParser.HanonymContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHanonymInner(final ProgramParser.HanonymInnerContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHanonymInner(final ProgramParser.HanonymInnerContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterAbstractees(final ProgramParser.AbstracteesContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitAbstractees(final ProgramParser.AbstracteesContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterInnerabstract(final ProgramParser.InnerabstractContext ctx) {
        this.startObject(ctx);
        this.objects.prop("abstract");
        this.objects.leave();
    }

    @Override
    public void exitInnerabstract(final ProgramParser.InnerabstractContext ctx) {
        // Nothing here
    }

    @Override
    public void enterAhead(final ProgramParser.AheadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitAhead(final ProgramParser.AheadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMethod(final ProgramParser.MethodContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMethod(final ProgramParser.MethodContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMethodNamed(final ProgramParser.MethodNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMethodNamed(final ProgramParser.MethodNamedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMethodAs(final ProgramParser.MethodAsContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMethodAs(final ProgramParser.MethodAsContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethod(final ProgramParser.HmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethod(final ProgramParser.HmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethodExtended(final ProgramParser.HmethodExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodExtended(final ProgramParser.HmethodExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethodVersioned(final ProgramParser.HmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodVersioned(final ProgramParser.HmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethodExtendedVersioned(
        final ProgramParser.HmethodExtendedVersionedContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void exitHmethodExtendedVersioned(
        final ProgramParser.HmethodExtendedVersionedContext ctx
    ) {
        // Nothing here
    }

    @Override
    public void enterHmethodHead(final ProgramParser.HmethodHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodHead(final ProgramParser.HmethodHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterHmethodHeadExtended(final ProgramParser.HmethodHeadExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitHmethodHeadExtended(final ProgramParser.HmethodHeadExtendedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethod(final ProgramParser.VmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethod(final ProgramParser.VmethodContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodVersioned(final ProgramParser.VmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodVersioned(final ProgramParser.VmethodVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodHead(final ProgramParser.VmethodHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodHead(final ProgramParser.VmethodHeadContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodTail(final ProgramParser.VmethodTailContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodTail(final ProgramParser.VmethodTailContext ctx) {
        // Nothing here
    }

    @Override
    public void enterVmethodTailVersioned(final ProgramParser.VmethodTailVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitVmethodTailVersioned(final ProgramParser.VmethodTailVersionedContext ctx) {
        // Nothing here
    }

    @Override
    public void enterMethodTail(final ProgramParser.MethodTailContext ctx) {
        // Nothing here
    }

    @Override
    public void exitMethodTail(final ProgramParser.MethodTailContext ctx) {
        this.objects.enter();
        this.objects.prop("method");
        this.objects.xprop("base", "concat('.',@base)");
        this.objects.xprop("pos", "@pos - 1");
        this.objects.leave();
    }

    @Override
    public void enterMethodTailVersioned(final ProgramParser.MethodTailVersionedContext ctx) {
        this.startObject(ctx);
        this.objects.prop("base", String.format(".%s", ctx.NAME().getText()));
        this.objects.prop("method");
    }

    @Override
    public void exitMethodTailVersioned(final ProgramParser.MethodTailVersionedContext ctx) {
        this.objects.leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterBeginner(final ProgramParser.BeginnerContext ctx) {
        this.startObject(ctx);
        if (ctx.data() == null) {
            final String base;
            if (ctx.XI() != null) {
                base = "$";
            } else if (ctx.STAR() != null) {
                base = "tuple";
                this.objects.prop("data", "tuple");
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
    public void exitBeginner(final ProgramParser.BeginnerContext ctx) {
        this.objects.leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterFinisher(final ProgramParser.FinisherContext ctx) {
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
    public void exitFinisher(final ProgramParser.FinisherContext ctx) {
        this.objects.leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterSpreadable(final ProgramParser.SpreadableContext ctx) {
        this.startObject(ctx);
        final String base;
        if (ctx.NAME() != null) {
            base = ctx.NAME().getText();
        } else if (ctx.AT() != null) {
            base = "@";
        } else if (ctx.RHO() != null) {
            base = "^";
        } else if (ctx.SIGMA() != null) {
            base = "&";
        } else {
            base = "";
        }
        if (!base.isEmpty()) {
            this.objects.prop("base", base);
        }
        if (ctx.COPY() != null) {
            this.objects.prop("copy");
        }
    }

    @Override
    public void exitSpreadable(final ProgramParser.SpreadableContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterFinisherCopied(final ProgramParser.FinisherCopiedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitFinisherCopied(final ProgramParser.FinisherCopiedContext ctx) {
        this.objects.enter();
        if (ctx.COPY() != null) {
            this.objects.prop("copy");
        }
        this.objects.leave();
    }

    @Override
    public void enterVersioned(final ProgramParser.VersionedContext ctx) {
        this.startObject(ctx);
        this.objects.prop("base", ctx.NAME().getText());
    }

    @Override
    public void exitVersioned(final ProgramParser.VersionedContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterReversed(final ProgramParser.ReversedContext ctx) {
        // Nothing here
    }

    @Override
    public void exitReversed(final ProgramParser.ReversedContext ctx) {
        this.objects.enter();
        this.objects.xprop("base", "concat('.',@base)");
        this.objects.leave();
    }

    @Override
    public void enterOname(final ProgramParser.OnameContext ctx) {
        // Nothing here
    }

    @Override
    public void exitOname(final ProgramParser.OnameContext ctx) {
        if (ctx.CONST() != null) {
            this.objects.enter();
            this.objects.prop("const");
            this.objects.leave();
        }
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterSuffix(final ProgramParser.SuffixContext ctx) {
        this.objects.enter();
        if (ctx.AT() != null) {
            this.objects.prop("name", ctx.AT().getText());
        } else if (ctx.NAME() != null) {
            this.objects.prop("name", ctx.NAME().getText());
        }
    }

    @Override
    public void exitSuffix(final ProgramParser.SuffixContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterScope(final ProgramParser.ScopeContext ctx) {
        this.objects.scope();
    }

    @Override
    public void exitScope(final ProgramParser.ScopeContext ctx) {
        this.objects.closeScope();
    }

    @Override
    public void enterScopeExtended(final ProgramParser.ScopeExtendedContext ctx) {
        this.objects.scope();
    }

    @Override
    public void exitScopeExtended(final ProgramParser.ScopeExtendedContext ctx) {
        this.objects.closeScope();
    }

    @Override
    public void enterVersion(final ProgramParser.VersionContext ctx) {
        if (ctx.VER() != null) {
            this.objects.prop("ver", ctx.VER().getText());
        }
    }

    @Override
    public void exitVersion(final ProgramParser.VersionContext ctx) {
        // Nothing here
    }

    @Override
    public void enterAs(final ProgramParser.AsContext ctx) {
        this.objects.enter();
        final String has;
        if (ctx.RHO() == null) {
            has = ctx.NAME().getText();
        } else {
            has = "^";
        }
        this.objects.prop("as", has);
    }

    @Override
    public void exitAs(final ProgramParser.AsContext ctx) {
        this.objects.leave();
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterData(final ProgramParser.DataContext ctx) {
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
                data = XeListener.bytesToHex((byte) 0x01);
            } else {
                data = XeListener.bytesToHex((byte) 0x00);
            }
        } else if (ctx.FLOAT() != null) {
            type = "bytes";
            base = "float";
            data = XeListener.bytesToHex(
                ByteBuffer
                    .allocate(Long.BYTES)
                    .putDouble(Double.parseDouble(text))
                    .array()
            );
        } else if (ctx.INT() != null) {
            type = "bytes";
            base = "int";
            data = XeListener.bytesToHex(
                ByteBuffer
                    .allocate(Long.BYTES)
                    .putLong(Long.parseLong(text))
                    .array()
            );
        } else if (ctx.HEX() != null) {
            type = "bytes";
            base = "int";
            data = XeListener.bytesToHex(
                ByteBuffer
                    .allocate(Long.BYTES)
                    .putLong(Long.parseLong(text.substring(2), 16))
                    .array()
            );
        } else if (ctx.STRING() != null) {
            type = "bytes";
            base = "string";
            data = XeListener.bytesToHex(
                StringEscapeUtils.unescapeJava(
                    text.substring(1, text.length() - 1)
                ).getBytes(StandardCharsets.UTF_8)
            );
        } else if (ctx.TEXT() != null) {
            type = "bytes";
            base = "string";
            final int indent = ctx.getStart().getCharPositionInLine();
            data = XeListener.bytesToHex(
                StringEscapeUtils.unescapeJava(
                    XeListener.trimMargin(text, indent)
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
    public void exitData(final ProgramParser.DataContext ctx) {
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
     */
    private void startObject(final ParserRuleContext ctx) {
        this.objects.start(
            ctx.getStart().getLine(),
            ctx.getStart().getCharPositionInLine()
        );
    }

    /**
     * Text source code.
     *
     * @param ctx Program context.
     * @return Original code.
     */
    private static String sourceText(final ProgramParser.ProgramContext ctx) {
        return ctx.getStart().getInputStream().getText(
            new Interval(
                ctx.getStart().getStartIndex(),
                ctx.getStop().getStopIndex()
            )
        );
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
