/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
            EoIndentLexerTest.TO_ADD_MESSAGE,
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
            EoIndentLexerTest.TO_ADD_MESSAGE,
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
            EoIndentLexerTest.TO_ADD_MESSAGE,
            new XMLDocument(new Xembler(objs).domQuietly()),
            XhtmlMatchers.hasXPaths(
                "/o/o",
                "/o[@z='a']"
            )
        );
    }

}
