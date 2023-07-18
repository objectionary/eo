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
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.cactoos.Text;
import org.cactoos.experimental.Threads;
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.IterableOf;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.Joined;
import org.cactoos.list.ListOf;
import org.cactoos.number.SumOf;
import org.cactoos.scalar.Reduced;
import org.cactoos.set.SetOf;
import org.cactoos.text.Replaced;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.hash.CommitHashesMap;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.Home;

/**
 * Mojo that replaces tags with hashes in XMIR.
 *
 * @since 0.29.6
 * @todo #1602:30min Don't rewrite parsed xmir. VersionsMojo is executed right
 *  after ParseMojo and rewrite xmir file with replaced tags with hashes in
 *  1-parse directory which is kind of wrong because files in the directory
 *  contains xmir right after parsing. Replacing tags with versions is kind of
 *  optimization. We either should create a new folder where files with replaced
 *  tags files are stored, find another way to catch wrong versions without
 *  touching files in 1-parse directory or just accept it and don't do anything
 *  since is not really critical.
 */
public final class VersionsMojo extends SafeMojo {
    /**
     * Tag pattern.
     */
    private static final Pattern SEMVER = Pattern.compile("[0-9]+\\.[0-9]+\\.[0-9]+");

    /**
     * Commit hashes map.
     */
    private final Map<String, CommitHash> hashes = new CommitHashesMap();

    @Override
    void exec() throws IOException {
        if (this.withVersions) {
            final Collection<ForeignTojo> tojos = this.scopedTojos().withXmir();
            final Path dir = this.targetDir.toPath();
            final String format = "ver=\"%s\"";
            final int total = new SumOf(
                new Threads<>(
                    Runtime.getRuntime().availableProcessors(),
                    new Mapped<>(
                        tojo -> () -> {
                            final Path path = tojo.xmir();
                            final Text source = new UncheckedText(new TextOf(path));
                            final List<Text> tags = new ListOf<>(
                                new Mapped<Text>(
                                    TextOf::new,
                                    new SetOf<>(
                                        new Filtered<>(
                                            ver -> !ver.isEmpty() && SEMVER.matcher(ver).matches(),
                                            new XMLDocument(path).xpath("//o[@ver]/@ver")
                                        )
                                    )
                                )
                            );
                            new Home(dir).save(
                                new Reduced<>(
                                    new IterableOf<>(
                                        new Joined<>(
                                            source,
                                            tags
                                        ).iterator()
                                    ),
                                    (src, tag) -> new Replaced(
                                        src,
                                        String.format(format, tag.asString()),
                                        String.format(
                                            format, this.hashes.get(tag.asString()).value()
                                        )
                                    )
                                ).value(),
                                dir.relativize(path)
                            );
                            return tags.size();
                        },
                        tojos
                    )
                )
            ).intValue();
            if (total > 0) {
                Logger.info(
                    this,
                    "Tags replaced with hashes %d in %d tojos",
                    total,
                    tojos.size()
                );
            } else {
                Logger.debug(
                    this,
                    "No tags replaced with hashes out of %d tojos",
                    tojos.size()
                );
            }
        }
    }
}
