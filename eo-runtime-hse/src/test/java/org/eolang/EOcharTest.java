package org.eolang;

import org.eolang.hse.EObool;
import org.eolang.hse.EOchar;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;


/**
 * Test cases for {@link EObool}
 */
class EOcharTest {

    /***
     * Test for datization
     * Checks if the data is returned
     */
    @Test
    void _getData() {
        final EOchar character = new EOchar('J');
        MatcherAssert.assertThat(
                character._getData().toChar(),
                Matchers.equalTo('J'));
    }

    /***
     * Test for {@code EOtoString}
     * checks if a character is returned as a string
     */
    @Test
    void EOtoString() {
        final EOchar character = new EOchar('J');
        MatcherAssert.assertThat(
                character.EOtoString()._getData().toString(),
                Matchers.equalTo("J")
        );
    }
}