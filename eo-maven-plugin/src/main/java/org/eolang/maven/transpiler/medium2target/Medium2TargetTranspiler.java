package org.eolang.maven.transpiler.medium2target;

import org.eolang.maven.transpiler.mediumcodemodel.EOSourceFile;
import org.eolang.maven.transpiler.mediumcodemodel.EOTargetFile;

import java.util.ArrayList;

public class Medium2TargetTranspiler {
    public static ArrayList<EOTargetFile> transpile(EOSourceFile file) {
        return file.transpile(null);
    }
}
