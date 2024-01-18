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
import com.jcabi.xml.XML;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Map;
import org.cactoos.io.InputOf;
import org.yaml.snakeyaml.Yaml;

/**
 * One test scenario of parsing and post-processing.
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
        Train<Shift> train = new ParsingTrain();
        if (xsls != null) {
            if (!map.containsKey("defaults")) {
                train = train.empty();
            }
            for (final String xsl : xsls) {
                train = train.with(new StClasspath(xsl));
            }
        }
        final XML out = new Xsline(train).pass(
            new EoSyntax(
                "scenario",
                new InputOf(String.format("%s\n", src))
            ).parsed()
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
