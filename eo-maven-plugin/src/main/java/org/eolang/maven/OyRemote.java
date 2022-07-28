/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Scanner;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * The simple HTTP Objectionary server.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals",
    "PMD.ConstructorOnlyInitializesOrCallOtherConstructors"})
public final class OyRemote implements Objectionary {

    /**
     * Tag.
     */
    private final String tag;

    /**
     * The address template.
     */
    private String template;

    /**
     * Constructor.
     * @param tag Tag
     */
    public OyRemote(final String tag) {
        this.tag = tag;
    }

    /**
     * Init class.
     * @return OyRemote
     * @throws IOException if fails
     */
    public OyRemote resolve() throws IOException {
        final String sha = this.getSha();
        this.template = String.format(
            // @checkstyle LineLength (1 line)
            "https://raw.githubusercontent.com/objectionary/home/%s/objects/%%s.eo",
            sha
        );
        return this;
    }

    @Override
    public String toString() {
        return this.template;
    }

    @Override
    public Input get(final String name) throws MalformedURLException {
        final URL url = new URL(
            String.format(this.template, name.replace(".", "/"))
        );
        Logger.debug(
            this, "The object '%s' will be pulled from %s...",
            name, url
        );
        return new InputOf(url);
    }

    /**
     * File downloader.
     * @param link Link to file
     * @param name Location
     * @throws IOException If fails
     */
    private static void download(final String link, final String name) throws IOException {
        final URL url = new URL(link);
        try (InputStream in = url.openStream()) {
            Files.copy(in, Paths.get(name));
        }
    }

    /**
     * Hash of head master.
     * @return SHA of commit
     * @throws IOException if fails
     * @checkstyle NonStaticMethodCheck (20 lines)
     */
    private String getSha() throws IOException {
        String sha = "master";
        final String link = String.format(
            "%s%s",
            "https://raw.githubusercontent.com/",
            "objectionary/home/gh-pages/tags.txt"
        );
        download(link, "tags.txt");
        final File file = new File(
            String.format(
                "%s/%s",
                System.getProperty("user.dir"),
                "tags.txt"
            )
        );
        final Scanner scanner = new Scanner(file);
        while (scanner.hasNextLine()) {
            final String line = scanner.nextLine();
            final String[] parts = line.split("\t");
            if (Objects.equals(parts[1], this.tag)) {
                sha = parts[0];
                Logger.info(this, "commit sha is %s", sha);
                break;
            }
        }
        file.delete();
        return sha;
    }

}
