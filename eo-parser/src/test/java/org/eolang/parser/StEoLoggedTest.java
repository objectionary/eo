package org.eolang.parser;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StFailure;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Queue;
import java.util.function.Consumer;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link StEoLogged}.
 *
 * @since 0.30
 */
class StEoLoggedTest {

    @Test
    void hasTheSameUid() {
        final StUnhex origin = new StUnhex();
        MatcherAssert.assertThat(
            String.format(
                "We expect, that the uid() calculation will be delegated to the origin: %s",
                origin
            ),
            new StEoLogged(origin).uid(),
            Matchers.equalTo(origin.uid())
        );
    }

    @Test
    void delegatesWithoutException() {
        final FakeLog log = new FakeLog();
        MatcherAssert.assertThat(
            "We expect that shift will successfully generate output xml",
            new StEoLogged(new StUnhex(), log).apply(1, StEoLoggedTest.example()),
            Matchers.notNullValue()
        );
        MatcherAssert.assertThat(
            String.format(
                "We expect that logs will be empty, but was %s",
                log.all()
            ),
            log.empty(),
            Matchers.is(true)
        );
    }

    @Test
    void printsMessageWithEoIfExceptionIsThrown() {
        final FakeLog log = new FakeLog();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new StEoLogged(new StFailure(), log).apply(1, StEoLoggedTest.example()),
            "We expect that shift will throw an exception, but it didn't"
        );
        MatcherAssert.assertThat(
            String.format(
                "We expect that logs will contain the eo representation of the xml, but was %s",
                log.all()
            ),
            log.last(),
            Matchers.containsString(
                String.join(
                    "\n",
                    "[] > main",
                    "  TRUE > x",
                    "  FALSE > y"
                )
            )
        );
    }

    private static XML example() {
        return new XMLDocument(
            String.join(
                "\n",
                "<program>",
                "  <errors/>",
                "  <sheets/>",
                "  <objects>",
                "    <o abstract=\"\" line=\"1\" name=\"main\" pos=\"0\">",
                "      <o base=\"bool\" data=\"bytes\" line=\"2\" name=\"x\" pos=\"2\">01</o>",
                "      <o base=\"bool\" data=\"bytes\" line=\"3\" name=\"y\" pos=\"2\">00</o>",
                "    </o>",
                "  </objects>",
                "</program>"
            )
        );
    }

    private static class FakeLog implements Consumer<String> {

        private final Queue<String> captured;

        FakeLog() {
            this(new LinkedList<>());
        }

        private FakeLog(final Queue<String> captured) {
            this.captured = captured;
        }

        @Override
        public void accept(final String message) {
            this.captured.add(message);
        }

        String last() {
            return this.captured.remove();
        }

        boolean empty() {
            return this.captured.isEmpty();
        }

        Collection<String> all() {
            return new ArrayList<>(this.captured);
        }
    }
}
