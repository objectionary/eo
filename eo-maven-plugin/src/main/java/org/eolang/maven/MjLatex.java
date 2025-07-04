/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.eolang.parser.OnDefault;

/**
 * Take .xmir files from target/eo/03-optimize directory and
 * generate .tex files for each of them in target/eo/latex directory.
 *
 * @since 0.29
 * @todo #1206:30min Create a summary file "target/eo/universe.tex".
 *  According to the <a href="https://github.com/objectionary/eo/issues/1206">issue</a>
 *  we need to generate summary in universe.tex file,
 *  which will include all generated objects. And this file
 *  should be a standalone compilable document.
 */
@Mojo(
    name = "latex",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjLatex extends MjSafe {

    /**
     * The directory where to generate to.
     */
    static final String DIR = "latex";

    /**
     * Latex extension (.tex).
     */
    static final String EXT = "tex";

    /**
     * Truncated the last part of the filename,
     * divided by dot.
     *
     * @param input Given string.
     * @return Last string after dot.
     */
    static String last(final String input) {
        final int index = input.lastIndexOf('.');
        final String last;
        if (index == -1 || index == input.length() - 1) {
            last = input;
        } else {
            last = input.substring(index + 1);
        }
        return last;
    }

    @Override
    void exec() throws IOException {
        for (final TjForeign tojo : this.scopedTojos().withXmir()) {
            final Path file = tojo.xmir();
            final Path dir = this.targetDir.toPath();
            final Path target = new Place(
                MjLatex.last(new OnDefault(new XMLDocument(file)).get())
            ).make(dir.resolve(MjLatex.DIR), MjLatex.EXT);
            new Saved(
                new LatexTemplate(
                    new Xnav(file).element("object").element("listing").text().get()
                ).asString(),
                target
            ).value();
            Logger.info(
                this,
                "Generated by LatexMojo %[file]s file (%[size]s)",
                target, target.toFile().length()
            );
        }
    }

}
