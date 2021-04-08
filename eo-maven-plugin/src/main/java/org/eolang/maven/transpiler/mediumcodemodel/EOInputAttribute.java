package org.eolang.maven.transpiler.mediumcodemodel;

import org.ainslec.picocog.PicoWriter;
import org.eolang.EOarray;
import org.eolang.core.EOObject;
import org.eolang.maven.transpiler.medium2target.TranslationCommons;

import java.util.ArrayList;

public class EOInputAttribute extends EOSourceEntity {

    private final boolean isVararg;
    private final String name;
    private final String targetName;

    public EOInputAttribute(String name, boolean isVararg) {
        this.name = name;
        this.isVararg = isVararg;
        this.targetName = "EO" + name;
    }

    public boolean isVararg() {
        return isVararg;
    }

    public String getName() {
        return name;
    }

    public String getTargetName() {
        return targetName;
    }

    public String getDescription() {
        return String.format("'%s' %sfree attribute", this.name, this.isVararg ? "variable-length " : "");
    }

    @Override
    public ArrayList<EOTargetFile> transpile(PicoWriter w) {
        TranslationCommons.bigComment(w, "Field for storing the " + getDescription() + ".");
        w.write("private final ");
        if (isVararg) {
            w.write(EOarray.class.getSimpleName());
        } else {
            w.write((EOObject.class.getSimpleName()));
        }
        w.writeln(String.format(" %s;", targetName));
        return new ArrayList<>();
    }
}
