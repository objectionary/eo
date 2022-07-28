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
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
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
     * Hash of head master.
     * @return SHA of commit
     * @throws IOException if fails
     * @checkstyle NonStaticMethodCheck (20 lines)
     */
    private String getSha() throws IOException {
        String sha = "master";
        final String command = String.format(
            "%s%s",
            "curl -o tags.txt https://raw.githubusercontent.com/",
            "objectionary/home/gh-pages/tags.txt"
        );
        try {
            Runtime.getRuntime().exec(command).waitFor();
        } catch (final InterruptedException exception) {
            Logger.info(this, exception.toString());
        }
        final File file = new File(
            String.format(
                "%s/%s",
                System.getProperty("user.dir"),
                "tags.txt"
            )
        );
        try (Scanner scanner = new Scanner(file)) {
            while (scanner.hasNextLine()) {
                final String line = scanner.nextLine();
                if (line.contains(this.tag)) {
                    sha = line.split("\t")[0];
                    Logger.info(
                        this, String.format("%s %s", "commit sha is", sha)
                    );
                    file.delete();
                    break;
                }
            }
        } catch (final FileNotFoundException exception) {
            file.delete();
            Logger.info(this, exception.toString());
            throw exception;
        }
        return sha;
    }

}
