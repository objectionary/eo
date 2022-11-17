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
package org.eolang.maven.testapi;

import com.yegor256.tojos.TjSmart;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.maven.Catalogs;
import org.eolang.maven.Home;
import org.eolang.maven.ParseMojo;
import org.eolang.maven.TranspileMojo;

/**
 * Maven compilation result.
 * @since 0.28.12
 */
public final class CompilationResult {
    /**
     * Compiled eo program id.
     */
    private final String id;

    /**
     * Target folder with compilation results.
     */
    private final Home target;

    /**
     * Tojo for eo-foreign.* file.
     */
    private final TjSmart foreign;

    /**
     * The main constructor.
     *
     * @param id Compiled eo program id
     * @param target Target folder with compilation results.
     * @param foreign Path to eo-foreign.* file
     */
    public CompilationResult(
        final String id,
        final Home target,
        final Path foreign
    ) {
        this.target = target;
        this.foreign = new TjSmart(
            Catalogs.INSTANCE.make(foreign)
        );
        this.id = id;
    }

    /**
     * Checks if eo program was successfully parsed to xmir file.
     * eo-foreign tojo also has an entry about it.
     *
     * @return True if eo program parsed to xmir successfully.
     */
    public boolean xmirCompiled() {
        return this.target.exists(
            Paths.get(
                String.format(
                    "%s/%s.%s",
                    ParseMojo.DIR,
                    this.id.replace(".", "/"),
                    TranspileMojo.EXT
                )
            )
        ) && this.foreign.getById(this.id).exists("xmir");
    }
}
