/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
 * @since 0.1
 * @checkstyle CyclomaticComplexityCheck (500 lines)
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 * @todo #348:30min Make changes to store INT as bytes.
 *  After that update this todo. When all data types
 *  are stored as bytes, remove data attribute from XML
 *  and XSLT templates.
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals"})
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
     * Redundancy checker.
     */
    private final RedundantParentheses check;

    /**
     * Ctor.
     * @param name Tha name of it
     * @param check The strategy to check eo expressions for redundant parentheses.
     */
    public XeListener(final String name, final RedundantParentheses check) {
        this.name = name;
        this.dirs = new Directives();
        this.objects = new Objects.ObjXembly();
        this.start = System.nanoTime();
        this.check = check;
    }

    @Override
    public void enterProgram(final ProgramParser.ProgramContext ctx) {
        this.dirs.add("program")
            .attr("name", this.name)
            .attr("version", Manifests.read("EO-Version"))
            .attr(
                "time",
                ZonedDateTime.now(ZoneOffset.UTC).format(
                    DateTimeFormatter.ISO_INSTANT
                )
            )
            .add("listing").set(XeListener.sourceText(ctx)).up()
            .add("errors").up()
            .add("sheets").up();
    }

    @Override
    public void exitProgram(final ProgramParser.ProgramContext ctx) {
        this.dirs
            .attr("ms", (System.nanoTime() - this.start) / (1000L * 1000L))
            .up();
    }

    @Override
    public void enterLicense(final ProgramParser.LicenseContext ctx) {
        this.dirs.add("license").set(
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
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterMetas(final ProgramParser.MetasContext ctx) {
        this.dirs.add("metas");
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
        // This method is created by ANTLR and can't be removed
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
        if (ctx.application() != null) {
            ProgramParser.ApplicationContext application = ctx.application();
            if (application.suffix() != null) {
                application = application.application();
            }
            final String text = application.getText();
            if (this.check.test(text)) {
                this.dirs.push()
                    .xpath("/program/errors")
                    .add("error")
                    .attr("line", ctx.getStart().getLine())
                    .attr("severity", "warning")
                    .set(String.format("'%s' contains redundant parentheses", text))
                    .pop();
            }
        }
    }

    @Override
    public void exitObject(final ProgramParser.ObjectContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterAbstraction(final ProgramParser.AbstractionContext ctx) {
        this.objects.start(
            ctx.getStart().getLine(),
            ctx.getStart().getCharPositionInLine()
        );
        this.objects.prop("abstract", "");
        if (ctx.SLASH() != null) {
            if (ctx.QUESTION() == null) {
                this.objects.prop("atom", ctx.NAME());
            } else {
                this.objects.prop("atom", "?");
            }
        }
        this.objects.leave();
    }

    @Override
    public void exitAbstraction(final ProgramParser.AbstractionContext ctx) {
        // Nothing here
    }

    @Override
    public void enterAttributes(final ProgramParser.AttributesContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void exitAttributes(final ProgramParser.AttributesContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterAttribute(final ProgramParser.AttributeContext ctx) {
        this.objects.enter();
        this.objects.start(
            ctx.getStart().getLine(),
            ctx.getStart().getCharPositionInLine()
        );
    }

    @Override
    public void exitAttribute(final ProgramParser.AttributeContext ctx) {
        this.objects.leave();
        this.objects.leave();
    }

    @Override
    public void enterLabel(final ProgramParser.LabelContext ctx) {
        if (ctx.AT() != null) {
            this.objects.prop("name", ctx.AT().getText());
        }
        if (ctx.NAME() != null) {
            this.objects.prop("name", ctx.NAME().getText());
        }
        if (ctx.DOTS() != null) {
            this.objects.prop("vararg", "");
        }
    }

    @Override
    public void exitLabel(final ProgramParser.LabelContext ctx) {
        // Nothing here
    }

    @Override
    public void enterTail(final ProgramParser.TailContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitTail(final ProgramParser.TailContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterSuffix(final ProgramParser.SuffixContext ctx) {
        this.objects.enter();
        if (ctx.CONST() != null) {
            this.objects.prop("const", "");
        }
    }

    @Override
    public void exitSuffix(final ProgramParser.SuffixContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterMethod(final ProgramParser.MethodContext ctx) {
        this.objects.start(
            ctx.getStart().getLine(),
            ctx.getStart().getCharPositionInLine()
        );
        if (ctx.COPY() != null) {
            this.objects.prop("copy", "");
        }
        this.objects.prop("method", "");
        this.objects.prop("base", String.format(".%s", ctx.mtd.getText()));
        this.objects.leave();
    }

    @Override
    public void exitMethod(final ProgramParser.MethodContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterScope(final ProgramParser.ScopeContext ctx) {
        this.objects.alias();
    }

    @Override
    public void exitScope(final ProgramParser.ScopeContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    @SuppressWarnings("PMD.ConfusingTernary")
    public void enterHead(final ProgramParser.HeadContext ctx) {
        this.objects.start(
            ctx.getStart().getLine(),
            ctx.getStart().getCharPositionInLine()
        );
        if (ctx.COPY() != null) {
            this.objects.prop("copy", "");
        }
        String base = "";
        if (ctx.NAME() != null) {
            base = ctx.NAME().getText();
        } else if (ctx.AT() != null) {
            base = "@";
        } else if (ctx.XI() != null) {
            base = "$";
        } else if (ctx.STAR() != null) {
            base = "array";
            this.objects.prop("data", "array");
        } else if (ctx.RHO() != null) {
            base = "^";
        } else if (ctx.ROOT() != null) {
            base = "Q";
        } else if (ctx.HOME() != null) {
            base = "QQ";
        } else if (ctx.SIGMA() != null) {
            base = "&";
        }
        if (ctx.DOT() != null) {
            base = String.format(".%s", base);
        }
        if (!base.isEmpty()) {
            this.objects.prop("base", base);
        }
    }

    @Override
    public void exitHead(final ProgramParser.HeadContext ctx) {
        if (ctx.DOTS() != null) {
            this.objects.prop("unvar", "");
        }
        this.objects.leave();
    }

    @Override
    public void enterHas(final ProgramParser.HasContext ctx) {
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
    public void exitHas(final ProgramParser.HasContext ctx) {
        this.objects.leave();
    }

    @Override
    public void enterApplication(final ProgramParser.ApplicationContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void exitApplication(final ProgramParser.ApplicationContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void enterHtail(final ProgramParser.HtailContext ctx) {
        this.objects.enter();
    }

    @Override
    public void exitHtail(final ProgramParser.HtailContext ctx) {
        this.objects.leave();
    }

    // @checkstyle ExecutableStatementCountCheck (100 lines)
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
            type = "float";
            base = "float";
            data = Double.toString(Double.parseDouble(text));
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
            type = "int";
            base = "int";
            data = Long.toString(
                Long.parseLong(text.substring(2), 16)
            );
        } else if (ctx.STRING() != null) {
            type = "string";
            base = "string";
            data = text.substring(1, text.length() - 1);
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
        this.objects.data(
            data
                .replace("\n", "\\n")
                .replace("\r", "\\r")
        );
    }

    @Override
    public void exitData(final ProgramParser.DataContext ctx) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void visitTerminal(final TerminalNode node) {
        // This method is created by ANTLR and can't be removed
    }

    @Override
    public void visitErrorNode(final ErrorNode node) {
        throw new ParsingException(
            node.getText(),
            new IllegalArgumentException(),
            0
        );
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
     * Help method.
     */
    private void enter() {
        this.dirs.xpath("o[last()]").strict(1);
    }

    /**
     * Text source code.
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
     * @param text Text block.
     * @param indent Indentation level.
     * @return Trimmed text.
     */
    private static String trimMargin(final String text, final int indent) {
        final String rexp = "\n\\s{%d}";
        String res = text
            .substring(3, text.length() - 3);
        res = res.replaceAll(String.format(rexp, indent), "\n");
        if (!res.isEmpty() && res.charAt(0) == '\n') {
            res = res.substring(1);
        }
        if (!res.isEmpty() && res.charAt(res.length() - 1) == '\n') {
            res = res.substring(0, res.length() - 1);
        }
        return res;
    }

    /**
     * Bytes to HEX.
     * @param bytes Bytes.
     * @return Hexadecimal value as string.
     */
    private static String bytesToHex(final byte... bytes) {
        final StringJoiner str = new StringJoiner(" ");
        for (final byte bty : bytes) {
            str.add(String.format("%02X", bty));
        }
        return str.toString();
    }
}
