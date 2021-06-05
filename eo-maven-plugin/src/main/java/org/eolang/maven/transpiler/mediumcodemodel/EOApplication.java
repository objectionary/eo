package org.eolang.maven.transpiler.mediumcodemodel;

import org.ainslec.picocog.PicoWriter;
import org.eolang.EOarray;
import org.eolang.core.EOObject;
import org.eolang.core.EOThunk;
import org.eolang.maven.transpiler.medium2target.TranslationCommons;

import java.util.ArrayList;
import java.util.Optional;

/***
 * Represents the medium code model for the application operation
 */
public class EOApplication extends EOSourceEntity {

    /***
     * The name of the object being applied
     */
    private final String appliedObject;

    /***
     * Arguments of the application (i.e., objects that are passed into the applied object)
     * May be absent
     */
    private ArrayList<EOApplication> arguments;

    /***
     * The scope in which the application is performed
     * Typically, the scope is the nearest abstraction that the application belongs to
     */
    private EOSourceEntity scope;

    /***
     * The name of the application in the EO source code (may be absent)
     */
    private final Optional<String> name;

    /***
     * The name of the application in the target Java source code (may be absent)
     */
    private final Optional<String> targetName;

    /***
     * If the application is a wrapper for some abstraction, the abstraction is stored here
     */
    private EOAbstraction wrappedAbstraction;

    /***
     * If the application copies standard data type object (like int or string) the data and its type are stored here
     */
    private Optional<EOData> data;

    /***
     * Does the application refer to
     * the {@code appliedObject} as an attribute
     * of the {@code dotNotationBase} object?
     */
    private final boolean isDotNotation;

    /***
     * The base object where the dot-notation based application accesses its attribute {@code appliedObject}
     * May be absent if the application is plain (i.e., not dot-notation based)
     */
    private EOApplication dotNotationBase;

    /***
     * Anonymous objects that are referenced in the scope of this application.
     */
    private ArrayList<EOAbstraction> anonymousObjects = new ArrayList<>();


    public EOApplication(boolean isDotNotation, String appliedObject, Optional<String> name, Optional<EOData> data) {
        this.isDotNotation = isDotNotation;
        this.dotNotationBase = dotNotationBase;
        this.appliedObject = appliedObject;
        this.arguments = arguments;
        this.name = name;
        if (name.isPresent()) {
            targetName = Optional.of("EO" + name.get());
        } else {
            targetName = Optional.empty();
        }
        this.data = data;
    }

    public EOAbstraction getWrappedAbstraction() {
        return wrappedAbstraction;
    }

    public void setWrappedAbstraction(EOAbstraction wrappedAbstraction) {
        if (this.wrappedAbstraction == null || this.wrappedAbstraction.getXmlName().equals(wrappedAbstraction.getXmlName())) {
            this.wrappedAbstraction = wrappedAbstraction;
        }
        else {
            throw new RuntimeException("application already has the wrapped abstraction");
        }
    }

    public EOSourceEntity getScope() {
        return scope;
    }

    public void setScope(EOSourceEntity scope) {
        this.scope = scope;
    }

    public boolean isDotNotation() {
        return isDotNotation;
    }

    public EOApplication getDotNotationBase() {
        return dotNotationBase;
    }

    public void setDotNotationBase(EOApplication dotNotationBase) {
        this.dotNotationBase = dotNotationBase;
    }

    public String getAppliedObject() {
        return appliedObject;
    }

    public ArrayList<EOApplication> getArguments() {
        return arguments;
    }

    public void setArguments(ArrayList<EOApplication> arguments) {
        this.arguments = arguments;
    }

    public Optional<String> getName() {
        return name;
    }

    public void addAnonymous(EOAbstraction anonymous) {
        if (anonymousObjects.stream().anyMatch(a -> a.getAnonymousName().equals(anonymous.getAnonymousName()))) {
            return;
        }
        anonymousObjects.add(anonymous);
        anonymous.setAnonymousName(String.format("anonymous$%d", anonymousObjects.size()));
    }

    private EOApplication isAccessible(String base) {
        EOAbstraction scope = (EOAbstraction) getScope();
        return scope.getBoundAttributes().stream().filter(a -> a.targetName.equals(base)).findAny().orElse(null);
    }

    private String aliasToNormalForm() {
        String[] components = appliedObject.split("\\.");
        components[components.length - 1] = "EO" + components[components.length - 1];
        String result = "";
        for (int i = 0; i < components.length; i++) {
            result += components[i];
            if (i != components.length - 1) {
                result += ".";
            }
        }
        return result;
    }

    private String getCorrectReference() {
        if (appliedObject.contains(".")) {
            if (appliedObject.equals("org.eolang.array")) {
                return String.format("new %s", EOarray.class.getSimpleName());
            }
            // some outer object with a fully qualified name
            return String.format("new %s", aliasToNormalForm());
        }

        EOAbstraction abstractionScope = (EOAbstraction) scope;
        if (abstractionScope.getXmlName().equals(appliedObject)) {
            // recursive reference
            return String.format("new %s", abstractionScope.getTargetName().get());
        }
        Optional<EOInputAttribute> attr = abstractionScope.getFreeAttributes().stream().filter(a -> a.getName().equals(appliedObject)).findAny();
        if (attr.isPresent()) {
            return String.format("this.%s", attr.get().getTargetName());
        }
        Optional<EOApplication> application = abstractionScope.getBoundAttributes().stream().filter(a -> a.getName().orElse("").equals(appliedObject)).findAny();
        if (application.isPresent()) {
            return String.format("this.%s", application.get().targetName.get());
        }

        while(!(abstractionScope.getScope() instanceof EOSourceFile)) {
            abstractionScope = (EOAbstraction) abstractionScope.getScope();
        }

        EOSourceFile eoSourceFileScope = (EOSourceFile) abstractionScope.getScope();
        for (EOAbstraction abstraction: eoSourceFileScope.getObjects()) {
            if (abstraction.getInstanceName().orElse("").equals(appliedObject)) {
                return String.format("new %s", abstraction.getTargetName().get());
            }
        }
        throw new RuntimeException(String.format("Cannot find referenced %s.", appliedObject));
    }

    @Override
    public String toString() {
        if (name.isPresent()) {
            return "application " + name.get();
        } else {
            return "anonymous application of the object " + appliedObject;
        }
    }

    /***
     * Transpiles higher level applications
     * (i.e., transpiles proper wrappers for them and delegates the rest to a recursive method)
     */
    @Override
    public ArrayList<EOTargetFile> transpile(PicoWriter w) {
        if (!targetName.isPresent()) {
            // unnamed applications (i.e. nested)
            // these do not need any wrapping methods for them
            transpileApplication(w);
        }
        else {
            // bound attribute
            transpileWrapperForBoundAttribute(w);
        }
        return new ArrayList<>();
    }

    // transpiles to a Java method
    private void transpileWrapperForBoundAttribute(PicoWriter w) {
        String methodName;
        String cachedFieldName;
        if (name.get().equals("@")) {
            // decoration
            methodName = "_decoratee";
            cachedFieldName = "_cachedDecoratee";
            transpileCachingField(w, cachedFieldName);
            TranslationCommons.bigComment(w, "Declares the decoratee of this object.");
            w.writeln("@Override");
        }
        else {
            // bound attribute of some type
            methodName = targetName.get();
            cachedFieldName = "_cached" + methodName;
            transpileCachingField(w, cachedFieldName);
        }

        boolean isDecoratee = name.get().equals("@");
        if (wrappedAbstraction != null) {
            // abstraction-based bound attribute
            if (!isDecoratee){
                TranslationCommons.bigComment(w, String.format("Abstraction-based bound attribute object '%s'", this.name.get()));
            }
            w.writeln_r(String.format("%s %s %s(%s) {", isDecoratee ? "protected" : "public", EOObject.class.getSimpleName(), methodName, wrappedAbstraction.getArgsString()));

            if(anonymousObjects.size() > 0) {
                TranslationCommons.bigComment(w, "Anonymous objects used in the scope of this method");
                for (EOAbstraction anonymous: anonymousObjects) {
                    anonymous.transpile(w);
                }
            }

            if (wrappedAbstraction.getInstanceName().get().equals("@")) {
                // anonymous-abstraction based decoratee (special case)
                transpileCachingRoutine(
                        w,
                        cachedFieldName,
                        () -> w.write(String.format("new %s(%s)",
                                wrappedAbstraction.getAnonymousName(),
                                wrappedAbstraction.getArgsString())));
            }
            else {
                transpileCachingRoutine(
                        w,
                        cachedFieldName,
                        () -> w.write(String.format("new %s(%s)",
                                wrappedAbstraction.getTargetName().get(),
                                wrappedAbstraction.getArgsString(false))));
            }
        }
        else {
            // application based bound attribute
            if (!isDecoratee){
                TranslationCommons.bigComment(w, String.format("Application-based bound attribute object '%s'", this.name.get()));
            }
            w.writeln_r(String.format("%s %s %s() {", isDecoratee ? "protected" : "public", EOObject.class.getSimpleName(), methodName));

            if(anonymousObjects.size() > 0) {
                TranslationCommons.bigComment(w, "Anonymous objects used in the scope of this method");
                for (EOAbstraction anonymous: anonymousObjects) {
                    anonymous.transpile(w);
                }
            }

            transpileCachingRoutine(w, cachedFieldName, () -> transpileApplication(w));
        }
        w.writeln_l("}");
    }

    private void transpileCachingField(PicoWriter w, String cachedFieldName) {
        if (wrappedAbstraction != null && !wrappedAbstraction.getFreeAttributes().isEmpty()) {
            // for bound abstractions, no caching is performed
        }
        else {
            w.writeln();
            TranslationCommons.bigComment(w, String.format("Field for caching the '%s' attribute.", this.name.get()));
            w.writeln(String.format("private EOObject %s = null;", cachedFieldName));
        }
    }

    private void transpileCachingRoutine(PicoWriter w, String cachedFieldName, Runnable returnExpression) {
        if (wrappedAbstraction != null && !wrappedAbstraction.getFreeAttributes().isEmpty()) {
            // for bound abstractions, no caching is performed
            w.write("return ");
            returnExpression.run();
            w.writeln(";");
        }
        else {
            w.writeln_r(String.format("if (%s == null) {", cachedFieldName));
            w.write(String.format("%s = ", cachedFieldName));
            returnExpression.run();
            w.writeln(";");
            w.writeln_l("}");
            w.writeln(String.format("return %s;", cachedFieldName));
        }
    }

    private void transpileApplication(PicoWriter w) {
        // anonymous-abstraction based application
        if (name.isEmpty() && wrappedAbstraction != null) {
            w.write(String.format("new %s(%s)", wrappedAbstraction.getAnonymousName(), wrappedAbstraction.getArgsString()));
            return;
        }
        // data stored inside application
        if (data.isPresent()) {
            data.get().transpile(w);
            return;
        }
        // dot-notation based application
        if (isDotNotation) {
            transpileDotNotationApplication(w);
            return;
        }
        // parent access
        if (appliedObject.equals("^")) {
            w.write("_getParentObject()");
            return;
        }
        // plain application
        w.write(getCorrectReference());
        w.write("(");
        if (arguments.size() > 0) {
            transpileArgs(w);
        }
        w.write(")");
    }


    private void transpileDotNotationApplication(PicoWriter w) {
        w.write("(");
        getDotNotationBase().transpileApplication(w);
        if (appliedObject.equals("^")) {
            w.write(")._getParentObject()");
        }
        else {
            w.write(")._getAttribute(");
            w.write(String.format("\"EO%s\"", appliedObject));
            if (arguments.size() > 0) {
                w.write(", ");
                transpileArgs(w);
            }
            w.write(")");
        }
    }

    private void transpileArgs(PicoWriter w) {
        for (int i = 0; i < arguments.size(); i++) {
            EOApplication arg = arguments.get(i);
            w.write(String.format("new %s(() -> (", EOThunk.class.getSimpleName()));
            arg.transpileApplication(w);
            w.write("))");
            if (i != arguments.size() - 1) {
                w.write(", ");
            }
        }
    }
}