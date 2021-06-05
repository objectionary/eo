package org.eolang.maven.transpiler.mediumcodemodel;

import com.google.googlejavaformat.java.Formatter;
import com.google.googlejavaformat.java.FormatterException;
import org.ainslec.picocog.PicoWriter;
import org.eolang.EOarray;
import org.eolang.core.EOObject;
import org.eolang.maven.transpiler.medium2target.TranslationCommons;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;

public class EOAbstraction extends EOSourceEntity {
    private final Optional<String> instanceName;

    private final ArrayList<EOInputAttribute> freeAttributes;
    private final String xmlName;
    private final ArrayList<EOAbstraction> subAbstractions;
    private final Optional<String> targetName;
    private ArrayList<EOApplication> boundAttributes;
    private EOSourceEntity scope;
    private String anonymousName;

    public EOAbstraction(String xmlName, Optional<String> instanceName, ArrayList<EOInputAttribute> freeAttributes) {
        this.xmlName = xmlName;
        this.instanceName = instanceName;
        this.freeAttributes = freeAttributes;
        this.subAbstractions = new ArrayList<>();
        if (instanceName.isPresent()) {
            targetName = Optional.of("EO" + instanceName.get());
        } else {
            targetName = Optional.empty();
        }
    }

    public void setApplications(ArrayList<EOApplication> applications) {
        this.boundAttributes = applications;
    }

    public void addSubAbstraction(EOAbstraction abstraction) {
        this.subAbstractions.add(abstraction);
    }

    public Optional<String> getInstanceName() {
        return instanceName;
    }

    public ArrayList<EOInputAttribute> getFreeAttributes() {
        return freeAttributes;
    }

    public String getXmlName() {
        return xmlName;
    }

    public ArrayList<EOApplication> getBoundAttributes() {
        return boundAttributes;
    }

    public ArrayList<EOAbstraction> getSubAbstractions() {
        return subAbstractions;
    }

    public EOSourceEntity getScope() {
        return scope;
    }

    public void setScope(EOSourceEntity scope) {
        this.scope = scope;
    }

    private String getScopeType() {
        if (scope instanceof EOSourceFile) {
            return "package";
        } else if (scope instanceof EOAbstraction) {
            if (!instanceName.isPresent() || instanceName.get().equals("@")) {
                return "anonymous";
            } else {
                return "attribute";
            }
        }
        return null;
    }

    public Optional<String> getTargetName() {
        return targetName;
    }

    private String getNestedChain() {
        EOAbstraction scope = (EOAbstraction) getScope();
        String format = String.format("attribute object '%s' \nof the", this.instanceName.get());
        while (true) {
            if (scope.getScopeType().equals("attribute")) {
                format = format + String.format(" attribute object '%s' \nof the", scope.instanceName.get());
                scope = (EOAbstraction) scope.getScope();
            } else if (scope.anonymousName != null) {
                format = format + String.format(" anonymous object with an assigned pseudo-name '%s'", scope.anonymousName);
                break;
            }
            else {
                format = format + String.format(" package-scope object '%s'", scope.instanceName.get());
                break;
            }
        }
        return format;
    }

    public String getArgsString() {
        return getArgsString(true);
    }
    public String getArgsString(boolean useTypes) {
        String result = "";
        for (int i = 0; i < freeAttributes.size(); i++) {
            EOInputAttribute attr = freeAttributes.get(i);
            String type = "";
            if (useTypes) {
                type = EOObject.class.getSimpleName();
            }
            if (attr.isVararg()) {
                type = type + "...";
            }
            if (useTypes) {
                type += " ";
            }
            result += String.format("%s%s", type, attr.getTargetName());
            if (i != freeAttributes.size() - 1) {
                result += ", ";
            }
        }
        return result;
    }

    public void setAnonymousName(String anonymousName) {
        this.anonymousName = anonymousName;
    }

    public String getAnonymousName() {
        return anonymousName;
    }

    @Override
    public String toString() {
        if (instanceName.isPresent()) {
            return "abstraction " + instanceName.get();
        } else {
            return "anonymous abstraction";
        }
    }

    @Override
    public ArrayList<EOTargetFile> transpile(PicoWriter parentWriter) {
        if (getScopeType().equals("package")) {
            EOSourceFile file = ((EOSourceFile) scope);
            PicoWriter w = new PicoWriter();

            transpileFileHeader(w, file);
            transpileClass(w);

            ArrayList<EOTargetFile> result = new ArrayList<>();
            String unformattedCode = w.toString();
            String formattedJava;
            try {
                formattedJava = new Formatter().formatSource(unformattedCode);
            } catch (FormatterException e) {
                throw new RuntimeException("Can't format the output");
            }
            result.add(new EOTargetFile(String.format("%s.java", this.targetName.get()), formattedJava));
            return result;
        } else {
            transpileClass(parentWriter);
            return new ArrayList<>();
        }
    }

    /***
     * Transpiles the header of the target Java file (i.e. package and import statements)
     */
    private void transpileFileHeader(PicoWriter w, EOSourceFile file) {
        /* package statement */
        w.writeln(String.format("package %s;", file.getEoPackage().getPackageName()));
        w.writeln("");
        /* import the base language objects */
        w.writeln(String.format("import org.eolang.*;"));
        w.writeln(String.format("import org.eolang.core.*;"));
        w.writeln(String.format("import java.util.function.Supplier;"));
        w.writeln("");
    }

    private void transpileClass(PicoWriter w) {
        if (getScopeType().equals("package")) {
            TranslationCommons.bigComment(w, String.format("Package-scope object '%s'.", this.instanceName.get()));
            w.writeln_r(String.format("public class %s extends %s {", this.targetName.get(), EOObject.class.getSimpleName()));
        } else if (getScopeType().equals("attribute")) {
            TranslationCommons.bigComment(w, (getNestedChain() + ".").replaceFirst("a", "A").split("\n"));
            if (getScope() instanceof EOAbstraction && ((EOAbstraction) getScope()).anonymousName!=null) {
                w.writeln_r(String.format("class %s extends %s {", this.targetName.get(), EOObject.class.getSimpleName()));
            }
            else {
                w.writeln_r(String.format("private class %s extends %s {", this.targetName.get(), EOObject.class.getSimpleName()));
            }
        }
        else {
            TranslationCommons.bigComment(w, String.format("Anonymous object with an assigned pseudo-name '%s'.", getAnonymousName()));
            w.writeln_r(String.format("class %s extends %s {", this.anonymousName, EOObject.class.getSimpleName()));

        }
        transpileClassContents(w);
        w.writeln_l("}");
        w.writeln("");
    }

    private void transpileClassContents(PicoWriter w) {
        if (freeAttributes.size() > 0) {
            w.writeln("");
            transpileFreeAttributes(w);
        }


        transpileConstructor(w);
        transpileParentObject(w);
        transpileDecoratee(w);
        transpileFreeAttrsGetters(w);
        if (boundAttributes.size() > 0) {
            transpileApplications(w);
        }
        if (subAbstractions.size() > 0) {
            transpileSubAbstractions(w);
        }
    }

    private void transpileFreeAttributes(PicoWriter w) {
        for (EOInputAttribute attr : freeAttributes) {
            attr.transpile(w);
        }
    }

    private void transpileConstructor(PicoWriter w) {
        w.writeln("");
        ArrayList<String> commentParams = new ArrayList<>();
        switch (getScopeType()) {
            case "package":
                commentParams.add(String.format("Constructs (via one-time-full application) the package-scope object '%s'.", this.instanceName.get()));
                break;
            case "attribute":
                commentParams.addAll(Arrays.asList(String.format("Constructs (via one-time-full application) the %s.", getNestedChain()).split("\n")));
                break;
            case "anonymous":
                commentParams.add(String.format("Constructs (via one-time-full application) the anonymous object with the pseudo-name '%s'.", this.anonymousName));
                break;
        }
        if (freeAttributes.size() > 0) {
            for (int i = 0; i < freeAttributes.size(); i++) {
                EOInputAttribute attr = freeAttributes.get(i);
                commentParams.add(String.format("@param %s the object to bind to the %s.", attr.getTargetName(), attr.getDescription()));
            }
        }
        TranslationCommons.bigComment(w, commentParams.toArray(String[]::new));
        if (getScopeType().equals("anonymous")) {
            w.write(String.format("public %s(", this.anonymousName));
        }
        else {
            w.write(String.format("public %s(", this.targetName.get()));
        }
        if (freeAttributes.size() == 0) {
            w.writeln(") {}");
        } else {
            w.write(getArgsString());
            w.writeln_r(") {");
            for (int i = 0; i < freeAttributes.size(); i++) {
                EOInputAttribute attr = freeAttributes.get(i);
                String wrapper;
                if (attr.isVararg()) {
                    wrapper = String.format("new %s(%s)", EOarray.class.getSimpleName(), attr.getTargetName());
                } else {
                    wrapper = attr.getTargetName();
                }
                w.writeln(String.format("this.%s = %s;", attr.getTargetName(), wrapper));
            }
            w.writeln_l("}");
        }

        w.writeln("");
    }

    private void transpileFreeAttrsGetters(PicoWriter w) {
        if (freeAttributes.size() > 0) {
            for (int i = 0; i < freeAttributes.size(); i++) {
                EOInputAttribute attr = freeAttributes.get(i);
                String type = attr.isVararg() ? EOarray.class.getSimpleName() : EOObject.class.getSimpleName();
                TranslationCommons.bigComment(w, String.format("Returns the object bound to the '%s' input attribute.", attr.getName()));
                w.writeln_r(String.format("public %s %s() {", type, attr.getTargetName()));
                w.writeln(String.format("return this.%s;", attr.getTargetName()));
                w.writeln_l("}");
                w.writeln("");
            }
        }
    }

    private void transpileParentObject(PicoWriter w) {
        if (!getScopeType().equals("package")) {
            EOAbstraction scope = (EOAbstraction) getScope();
            if (scope.anonymousName != null) {
                TranslationCommons.bigComment(w, String.format("Declares the parent object '%s' of this object.", scope.anonymousName));
            }
            else {
                TranslationCommons.bigComment(w, String.format("Declares the parent object '%s' of this object.", scope.getInstanceName().get()));
            }
            w.writeln("@Override");
            w.writeln_r(String.format("protected %s _parent() {", EOObject.class.getSimpleName()));
            if (scope.anonymousName != null) {
                w.writeln(String.format("return %s.this;", scope.anonymousName));
            }
            else {
                w.writeln(String.format("return %s.this;", scope.targetName.get()));
            }
            w.writeln_l("}");
            w.writeln("");
        }
    }

    private void transpileDecoratee(PicoWriter w) {
        Optional<EOApplication> decoratee = boundAttributes.stream().filter(o -> o.getName().orElse("").equals("@")).findFirst();
        if (decoratee.isPresent()) {
            decoratee.get().transpile(w);
            // remove it to avoid double transpilation
            boundAttributes.remove(decoratee.get());
            w.writeln("");
        }
    }


    private void transpileSubAbstractions(PicoWriter w) {
        for (int i = 0; i < subAbstractions.size(); i++) {
            subAbstractions.get(i).transpile(w);
            w.writeln("");
        }
    }

    private void transpileApplications(PicoWriter w) {
        for (int i = 0; i < boundAttributes.size(); i++) {
            boundAttributes.get(i).transpile(w);
            w.writeln("");
        }
    }
}
