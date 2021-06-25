package org.eolang;

import org.eolang.hse.EOstring;
import org.eolang.hse.core.data.EODataObject;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link EOstring}
 */
class EOstringTest {

    /***
     * Test for datization
     * Checks if the data is returned
     */
    @Test
    void _getData() {
        EOstring string = new EOstring("Hello");
        MatcherAssert.assertThat(string._getData().toString(), Matchers.equalTo("Hello"));
    }

    /***
     * Test for {@code EOtrim}
     * checks if a string value with spaces on sides gets trimmed
     */
    @Test
    void EOtrim() {
        EOstring string = new EOstring(" Hello ");
        MatcherAssert.assertThat(
                string.EOtrim()._getData().toString(),
                Matchers.equalTo("Hello")
        );
    }

    /***
     *Test for {@code EOtoInt}
     * checks if the int value of a string number is returned
     */
    @Test
    void toInt() {
        EOstring string = new EOstring("12");
        MatcherAssert.assertThat(string.EOtoInt()._getData().toInt(), Matchers.equalTo(12L));
    }

    /***
     *Test for {@code EOeq}
     * checks if the base string value is equal to another string
     */
    @Test
    void EOeq() {
        EOstring string = new EOstring("Hello");
        MatcherAssert.assertThat(
                string.EOeq(
                        new EODataObject("Hello")
                )._getData().toBoolean(),
                Matchers.equalTo(true)
        );
    }
}