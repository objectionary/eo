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

import com.jcabi.log.Logger;
import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.yaml.snakeyaml.Yaml;

/**
 * Check that XSL transforms right.
 *
 * @since 1.0
 * @checkstyle AbbreviationAsWordInNameCheck (5 lines)
 */
public final class CheckXSL {

    /**
     * Home for XSLs.
     */
    private final Path home;

    /**
     * The config in YAML.
     */
    private final String script;

    /**
     * Ctor.
     * @param path Home path
     * @param scrpt The script
     */
    public CheckXSL(final Path path, final String scrpt) {
        this.home = path;
        this.script = scrpt;
    }

    /**
     * Make a run and return true if all is good.
     *
     * @return List of XPath expressions that failed
     * @throws IOException If fails
     */
    @SuppressWarnings("unchecked")
    public boolean isValid() throws IOException {
        final Yaml yaml = new Yaml();
        final Map<String, Object> map = yaml.load(this.script);
        final XML input = new XMLDocument(map.get("input").toString());
        final XML output = new XMLDocument(map.get("output").toString());
        final XSL xsl = new XSLDocument(
            this.home.resolve(map.get("xsl").toString())
        ).with(new ClasspathSources());
        final XML after = new XMLDocument(xsl.applyTo(input));
        final boolean matches = after.toString().equals(output.toString());
        if (!matches) {
            Logger.info(
                this, "Incorrect XML produced:\n%s\nExpected:\n%s",
                after, output
            );
        }
        return matches;
    }

}
