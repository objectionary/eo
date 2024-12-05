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

import com.jcabi.log.Logger;
import com.jcabi.xml.StrictXML;
import com.jcabi.xml.XML;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StXSL;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import org.cactoos.io.InputOf;
import org.yaml.snakeyaml.Yaml;

/**
 * One test scenario of parsing and post-processing.
 *
 * <p>This class is responsible for running a single test scenario
 * and checking if the output is correct. The scenario is defined
 * in YAML format. The class is able to run the scenario and
 * return the list of XPath expressions that don't pass after the run.</p>
 *
 * <p>The YAML format is as follows:</p>
 *
 * <pre>
 *     eo: |
 *       [] > foo
 *         QQ.io.stdout > @
 *           "Hello, world!"
 *     xsls:
 *     - file://path/to/xsl1.xsl
 *     - file://path/to/xsl2.xsl
 *     tests:
 *     - /xpath/to/element
 *     - /xpath/to/another/element
 *     skip: false
 * </pre>
 *
 * <p>The {@code eo} key contains the EO code to be parsed. The {@code xsls}
 * key contains a list of XSL files to be applied to the parsed XML. The {@code tests}
 * key contains a list of XPath expressions to be checked. The {@code skip} key
 * is a boolean flag that indicates if the test should be skipped. The {@code defaults}
 * key is a boolean flag that indicates if the default XSL files should be applied.</p>
 *
 * @since 1.0
 */
public final class CheckPack {

    /**
     * The scenario in YAML.
     */
    private final String script;

    /**
     * Ctor.
     * @param scrpt The script
     */
    public CheckPack(final String scrpt) {
        this.script = scrpt;
    }

    /**
     * Make a run and return the list of XPath expressions
     * that don't pass after the run.
     *
     * @return List of XPath expressions that failed
     * @throws IOException If fails
     */
    @SuppressWarnings("unchecked")
    public Collection<String> failures() throws IOException {
        final Yaml yaml = new Yaml();
        final Map<String, Object> map = yaml.load(this.script);
        final String src = map.get("eo").toString();
        final Iterable<String> xsls = (Iterable<String>) map.get("xsls");
        Train<Shift> train = new TrParsing();
        if (xsls != null) {
            train = train.empty();
            for (final String xsl : xsls) {
                if (xsl.startsWith("file://")) {
                    train = train.with(
                        new StXSL(
                            new XSLDocument(Paths.get(xsl.substring(7)))
                        )
                    );
                } else {
                    train = train.with(new StClasspath(xsl));
                }
            }
        }
        final XML out = new StrictXML(
            new Xsline(train).pass(
                new StrictXML(
                    new EoSyntax(
                        "scenario",
                        new InputOf(String.format("%s\n", src))
                    ).parsed()
                )
            )
        );
        Logger.debug(this, "Output XML:\n%s", out);
        final Collection<String> failures = new LinkedList<>();
        for (final String xpath : (Iterable<String>) map.get("tests")) {
            if (out.nodes(xpath).isEmpty()) {
                failures.add(xpath);
            }
        }
        if (!failures.isEmpty()) {
            Logger.info(this, "Broken XML:\n%s", out);
            Logger.info(this, "Broken EO:\n%s", src);
            Logger.info(this, "Failed XPath expressions:\n%[list]s", failures);
        }
        return failures;
    }

    /**
     * Is this check disabled?
     * @return True if disabled.
     */
    public boolean skip() {
        return new Yaml()
            .<Map<String, Boolean>>load(this.script)
            .getOrDefault("skip", Boolean.FALSE)
            .equals(Boolean.TRUE);
    }
}
