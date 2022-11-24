package org.eolang.parser;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

//todo:rename
public class PriorityTest {

    //todo:rename
    @Test
    public void checkPriority() throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Syntax syntax = new Syntax("", new InputOf(program()),
            new OutputTo(baos)
        );
        syntax.parse();
        Assertions.assertEquals(
            expectedNew(),
            new XMLDocument(baos.toByteArray())
                .nodes("/program/objects")
                .get(0)
                .toString()
        );
    }

    private String expectedOld() {
        return "<objects>\n"
            + "   <o abstract=\"\" line=\"1\" name=\"x\" pos=\"0\">\n"
            + "      <o base=\"int\" data=\"bytes\" line=\"2\" pos=\"2\">00 00 00 00 00 00 00 01</o>\n"
            + "      <o base=\".times\" line=\"2\" method=\"\" pos=\"3\">\n"
            + "         <o base=\"int\" data=\"bytes\" line=\"2\" pos=\"10\">00 00 00 00 00 00 00 02</o>\n"
            + "         <o alias=\"1\" base=\"int\" data=\"bytes\" line=\"2\" pos=\"13\">00 00 00 00 00 00 00 01</o>\n"
            + "         <o base=\".plus\" line=\"2\" method=\"\" pos=\"14\">\n"
            + "            <o base=\"other\" line=\"2\" pos=\"20\"/>\n"
            + "         </o>\n"
            + "         <o base=\".value\" line=\"2\" method=\"\" pos=\"25\"/>\n"
            + "      </o>\n"
            + "   </o>\n"
            + "</objects>\n";
    }

    private String expectedNew() {
        return "<objects>\n"
            + "   <o abstract=\"\" line=\"1\" name=\"x\" pos=\"0\">\n"
            + "      <o base=\"int\" data=\"bytes\" line=\"2\" pos=\"2\">00 00 00 00 00 00 00 01</o>\n"
            + "      <o base=\".times\" line=\"2\" method=\"\" pos=\"3\">\n"
            + "         <o base=\"int\" data=\"bytes\" line=\"2\" pos=\"10\">00 00 00 00 00 00 00 02</o>\n"
            + "         <o alias=\"1\" base=\"int\" data=\"bytes\" line=\"2\" pos=\"13\">00 00 00 00 00 00 00 01</o>\n"
            + "         <o base=\".plus\" line=\"2\" method=\"\" pos=\"14\">\n"
            + "            <o base=\"other\" line=\"2\" pos=\"20\"/>\n"
            + "            <o base=\".value\" line=\"2\" method=\"\" pos=\"25\"/>\n"
            + "         </o>\n"
            + "      </o>\n"
            + "   </o>\n"
            + "</objects>\n";
    }

    private String program() {
        return "[] > x\n  1.times 2 (1.plus other.value)";
    }
}
