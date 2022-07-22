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
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * The simple HTTP Objectionary server.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals",
    "PMD.ConstructorOnlyInitializesOrCallOtherConstructors"})
public final class OyRemote implements Objectionary {

    /**
     * The address template.
     */
    private final String template;

    /**
     * Ctor.
     * @param hash The GitHub hash
     * @throws JSONException if fails
     * @throws IOException if fails
     */
    public OyRemote(final String hash) throws IOException, JSONException {
        try {
            this.template = String.format(
                // @checkstyle LineLength (1 line)
                "https://raw.githubusercontent.com/objectionary/home/%s/objects/%%s.eo",
                this.getSha(hash)
            );
        } catch (final IOException | JSONException exception) {
            Logger.info(this, exception.toString());
            Logger.info(this, "Couldn't get commit SHA. It will be set as \"master\"");
            throw exception;
        }
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
     * @param hash String "master"
     * @return SHA of commit
     * @throws JSONException if fails
     * @throws IOException if fails
     * @checkstyle NonStaticMethodCheck (20 lines)
     */
    private String getSha(final String hash) throws IOException, JSONException {
        String sha = "master";
        try {
            final String query = String.format(
                // @checkstyle LineLength (1 line)
                "https://api.github.com/repos/objectionary/home/git/refs/heads/%s",
                hash
            );
            final JSONObject obj = OyRemote.readJsonFromUrl(query);
            sha = obj.getJSONObject("object").getString("sha");
        } catch (final IOException | JSONException exception) {
            Logger.info(this, exception.toString());
            Logger.info(this, "Couldn't get commit SHA. It will be set as \"master\"");
            throw exception;
        }
        return sha;
    }

    /**
     * Reader.
     * @param reader Reader
     * @return String
     * @throws IOException if something went wrong
     */
    private static String readAll(final Reader reader) throws IOException {
        final StringBuilder sbuilder = new StringBuilder();
        int cur;
        while (true) {
            cur = reader.read();
            if (cur == -1) {
                break;
            }
            sbuilder.append((char) cur);
        }
        return sbuilder.toString();
    }

    /**
     * Function from reading by url.
     * @param url URL
     * @return JSONobject
     * @throws IOException exception
     * @throws JSONException exception
     */
    private static JSONObject readJsonFromUrl(final String url) throws IOException, JSONException {
        final InputStream istream = new URL(url).openStream();
        try (BufferedReader reader = new BufferedReader(
            new InputStreamReader(istream, StandardCharsets.UTF_8)
        )) {
            final String json = OyRemote.readAll(reader);
            return new JSONObject(json);
        } finally {
            istream.close();
        }
    }

}
