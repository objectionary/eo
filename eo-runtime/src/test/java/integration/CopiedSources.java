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
package integration;

import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.stream.Collectors;
import org.cactoos.Func;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Sources copied from target current Farea working directory.
 * @since 0.43
 */
@SuppressWarnings({"JTCOP.RuleCorrectTestName", "JTCOP.RuleAllTestsHaveProductionClass"})
final class CopiedSources implements Func<String, CopiedSources> {
    /**
     * Farea instance.
     */
    private final Farea farea;

    /**
     * Ctor.
     * @param farea Farea
     */
    CopiedSources(final Farea farea) {
        this.farea = farea;
    }

    @Override
    public CopiedSources apply(final String target) throws IOException {
        final Path runtime = Paths.get(System.getProperty("user.dir")).resolve(target);
        final Collection<Path> sources = Files.walk(runtime)
            .filter(src -> !src.toFile().isDirectory())
            .collect(Collectors.toList());
        for (final Path src : sources) {
            this.farea.files()
                .file(String.format("%s/%s", target, runtime.relativize(src)))
                .write(new UncheckedText(new TextOf(src)).asString());
        }
        return this;
    }
}
