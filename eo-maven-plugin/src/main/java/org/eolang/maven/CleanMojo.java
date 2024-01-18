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
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.eolang.maven.util.Rel;

/**
 * Implementation of maven clean plugin,
 * just deleting target/eo directory.
 *
 * @since 0.28.6
 */
@Mojo(
    name = "clean",
    defaultPhase = LifecyclePhase.CLEAN,
    threadSafe = true
)
public class CleanMojo extends SafeMojo {

    @Override
    final void exec() {
        if (!this.targetDir.exists()) {
            Logger.debug(
                this,
                "Directory '%s' doesn't exist",
                new Rel(this.targetDir)
            );
            return;
        }
        if (this.purge(this.targetDir)) {
            Logger.info(
                this,
                "Deleted all files in: '%s'",
                new Rel(this.targetDir)
            );
        }
    }

    /**
     * Recursive deletion.
     *
     * @param dir Directory to be deleted
     * @return State {@code true} if deleted, {@code false} otherwise
     */
    private boolean purge(final File dir) {
        final File[] contents = dir.listFiles();
        if (null != contents) {
            for (final File file : contents) {
                this.purge(file);
            }
        }
        final boolean state = dir.delete();
        if (state) {
            Logger.debug(
                this,
                "'%s' purged",
                new Rel(dir)
            );
        }
        return state;
    }
}
