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

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Xembler;

/**
 * Test for {@link Objects}.
 * @since 0.1
 */
final class ObjectsTest {

    @Test
    void parsesOneObject() {
        final Objects objs = new Objects.ObjXembly();
        objs.start(9, 10);
        objs.prop("x", "y");
        objs.data("xxx");
        objs.leave();
        MatcherAssert.assertThat(
            new XMLDocument(new Xembler(objs).domQuietly()),
            XhtmlMatchers.hasXPaths(
                "/o",
                "/o[@line='9']",
                "/o[@pos='10']",
                "/o[@x='y']",
                "/o[text()='xxx']"
            )
        );
    }

    @Test
    void parsesNestedObjects() {
        final Objects objs = new Objects.ObjXembly();
        objs.start(1, 2);
        objs.start(3, 4);
        objs.prop("x", "y");
        objs.data("yyy");
        objs.leave();
        objs.leave();
        MatcherAssert.assertThat(
            new XMLDocument(new Xembler(objs).domQuietly()),
            XhtmlMatchers.hasXPaths(
                "/o",
                "/o/o[text()='yyy']"
            )
        );
    }

    @Test
    void parsesObjectsWithEnteringPrevious() {
        final Objects objs = new Objects.ObjXembly();
        objs.start(5, 6);
        objs.start(7, 8);
        objs.leave();
        objs.leave();
        objs.enter();
        objs.prop("z", "a");
        MatcherAssert.assertThat(
            new XMLDocument(new Xembler(objs).domQuietly()),
            XhtmlMatchers.hasXPaths(
                "/o/o",
                "/o[@z='a']"
            )
        );
    }

}
