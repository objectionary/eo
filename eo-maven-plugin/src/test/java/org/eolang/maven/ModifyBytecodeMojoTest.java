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

package org.eolang.maven;

import org.cactoos.set.SetOf;
import org.eolang.maven.util.Walk;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;

import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

public class ModifyBytecodeMojoTest {

    public static final Path SRC =
            Paths.get("src/test/resources/org/eolang/maven/bytecode-modification/java-files" );
    public static final String EXTENSION_JAVA = ".java";
    private static final Path RELATIVE_INPUT_DIR = Paths.get("target/classes" );
    private static final Path RELATIVE_OUTPUT_DIR = Paths.get("target/modified-classes" );
    private static final String HASH = "qwerty";
    private static final Set<String> GLOB_JAVA_FILES = new SetOf<>("**/*.java" );

    private static final String SUPER_CLASS_DEFAULT_CHECK = "SUPER_CLASS_DEFAULT_CHECK";
    private static final String INTERFACES_DEFAULT_CHECK = "INTERFACES_DEFAULT_CHECK";
    private static final String MODULE_DEFAULT_CHECK = "MODULE_DEFAULT_CHECK";
    private static final String OUTER_CLASS_DEFAULT_CHECK = "OUTER_CLASS_DEFAULT_CHECK";
    private static final String VISIBLE_ANNOTATIONS_DEFAULT_CHECK =
            "VISIBLE_ANNOTATIONS_DEFAULT_CHECK";
    private static final String INVISIBLE_ANNOTATIONS_DEFAULT_CHECK =
            "INVISIBLE_ANNOTATIONS_DEFAULT_CHECK";
    private static final String VISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK =
            "VISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK";
    private static final String INVISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK =
            "INVISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK";
    private static final String ATTRS_DEFAULT_CHECK = "ATTRS_DEFAULT_CHECK";
    private static final String INNER_CLASSES_DEFAULT_CHECK = "INNER_CLASSES_DEFAULT_CHECK";
    private static final String NEST_HOST_CLASS_DEFAULT_CHECK = "NEST_HOST_CLASS_DEFAULT_CHECK";
    private static final String NEST_MEMBERS_DEFAULT_CHECK = "NEST_MEMBERS_DEFAULT_CHECK";

    private static final char ASM_SLASH = '/';
    private static final String ASM_OBJECT = "java/lang/Object";
    private static final String INPUT_ASM_A = "org/eolang/A";
    private static final String ASM_DESC_A = asmNameToAsmDesc(INPUT_ASM_A, true);
    private static final String INPUT_ASM_B = "org/eolang/B";
    private static final String ASM_DESC_B = asmNameToAsmDesc(INPUT_ASM_B, true);
    private static final String INPUT_ASM_C = "org/eolang/C";
    private static final String ASM_DESC_C = asmNameToAsmDesc(INPUT_ASM_C, false);
    private static final String INPUT_ASM_VERSIONIZED = "org/eolang/Versionized";
    private static final String ASM_DESC_VERSIONIZED =
            asmNameToAsmDesc(INPUT_ASM_VERSIONIZED, false);

    private static final String INPUT_ASM_ANNOTATION_WITH_VERSIONIZED =
            "org/eolang/other/AnnotationWithVersionized";
    private static final String ASM_DESC_ANNOTATION_TARGET = "Ljava/lang/annotation/Target;";
    private static final String ASM_DESC_ANNOTATION_RETENTION = "Ljava/lang/annotation/Retention;";
    private static final String ASM_DESC_INTERFACE_ANNOTATION = "java/lang/annotation/Annotation";
    private static final String INPUT_ASM_INTERFACE = "org/eolang/other/Interface";
    private static final String INPUT_ASM_TEST_INNER_CLASS = "org/eolang/other/TestInnerClass";
    private static final String INPUT_ASM_INNER_CLASS =
            "org/eolang/other/TestInnerClass$InnerClass";
    private static final String INPUT_ASM_TEST_STATIC_NESTED_CLASS =
            "org/eolang/other/TestStaticNestedClass";
    private static final String INPUT_ASM_STATIC_NESTED_CLASS =
            "org/eolang/other/TestStaticNestedClass$StaticNestedClass";
    private static final String INPUT_ASM_TEST_INNER_CLASS_2 = "org/eolang/other/TestInnerClass2";
    private static final String INPUT_ASM_INNER_CLASS_2 =
            "org/eolang/other/TestInnerClass2$InnerClass2";

    private static String asmNameToAsmDesc(String asmName, boolean isVersionized) {
        StringBuilder sb = new StringBuilder("L" );
        if (isVersionized) {
            sb.append(HASH + ASM_SLASH);
        }
        sb.append(asmName);
        return sb.append(";" ).toString();
    }

    /**
     * 1. Read the special .java files from the resources path.
     * <br>
     * 2. Compile it and save binaries to the input directory.
     * <br>
     * 3. Create {@code Set<String> inputAsmNames}. The key is a relative path to .class file without file extension -
     * this format is convenient for using ASM library.
     * <br>
     * 4. If an input class doesn't have {@code org.eolang.Versionized} annotation AND doesn't contain usage of any
     * class that has {@code org.eolang.Versionized} annotation THEN remove the corresponding item MANUALLY from the set
     * {@code inputAsmNames} via method {@link ModifyBytecodeMojoTest#removeUnmodifiedClasses(Set)}.
     * <br>
     * 5. Execute the {@link ModifyBytecodeMojo}.
     * <br>
     * 6. Create {@code Collection<Path> outputPaths} with paths to all binaries in the output directory.
     * <br>
     * 7. Match the input files and the output files. This operation made inside the 'for' loop as well as other
     * important operations: 1) as soon as a match between two files was defined, explore and check the output file via
     * ASM library. 2) After this remove the corresponding two files: the one from {@code inputAsmNames} and the one
     * from {@code outputPaths}.
     * <br>
     * 8. If the {@code inputAsmNames} or the {@code outputPaths} still retain any items - assert that the test failed.
     * <br>
     * <br>
     * The methods {@link ModifyBytecodeMojoTest#checkFileA(String, Collection, Path)},
     * {@link ModifyBytecodeMojoTest#checkFileB(String, Collection, Path)},
     * {@link ModifyBytecodeMojoTest#checkFileC(String, Collection, Path)} is used for test cases from
     * <a href="https://github.com/objectionary/eo/issues/3091">the issue description</a>
     * <br>
     * Some additional information about other test cases can be found in the comments for the methods which started
     * with {@code checkFile...}
     */
    @Test
    public void bigIntegrationTest(@TempDir final Path temp) throws Exception {
        Path inputDirPath = temp.resolve(RELATIVE_INPUT_DIR);
        Path outputDirPath = temp.resolve(RELATIVE_OUTPUT_DIR);

        compile(inputDirPath);

        Set<String> inputAsmNames = new Walk(inputDirPath)
                .includes(ModifyBytecodeMojo.GLOB_CLASS_FILES)
                .stream()
                .map(path -> ModifyBytecodeMojo.pathToAsmName(path, inputDirPath))
                .collect(Collectors.toSet());

        removeUnmodifiedClasses(inputAsmNames);

        new FakeMaven(temp)
                .with("inputDir", inputDirPath)
                .with("outputDir", outputDirPath)
                .with("hash", HASH)
                .execute(ModifyBytecodeMojo.class);

        Collection<Path> outputPaths = new Walk(outputDirPath)
                .includes(ModifyBytecodeMojo.GLOB_CLASS_FILES);
        Set<String> unfoundInputAsmNames = new HashSet<>(inputAsmNames);
        for (String inputAsmName : inputAsmNames) {
            switch (inputAsmName) {
                case INPUT_ASM_A:
                    checkFileA(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_B:
                    checkFileB(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_C:
                    checkFileC(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_ANNOTATION_WITH_VERSIONIZED:
                    checkFileAnnotationWithVersionized(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_INTERFACE:
                    checkFileInterfaceUsage(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_INNER_CLASS:
                    checkFileInnerClass(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_TEST_STATIC_NESTED_CLASS:
                    checkFileTestStaticNestedClass(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_STATIC_NESTED_CLASS:
                    checkFileStaticNestedClass(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_TEST_INNER_CLASS_2:
                    checkFileTestInnerClass2(inputAsmName, outputPaths, outputDirPath);
                    break;
                case INPUT_ASM_INNER_CLASS_2:
                    checkFileInnerClass2(inputAsmName, outputPaths, outputDirPath);
                    break;
                default:
                    MatcherAssert.assertThat(
                            "Unexpected input file: " + inputAsmName,
                            true,
                            Matchers.equalTo(false)
                    );
            }
            unfoundInputAsmNames.remove(inputAsmName);
        }

        MatcherAssert.assertThat(
                "Can't check input classes: " + unfoundInputAsmNames,
                unfoundInputAsmNames.isEmpty(),
                Matchers.equalTo(true)
        );

        MatcherAssert.assertThat(
                "Irrelevant output classes: " + outputPaths,
                outputPaths.isEmpty(),
                Matchers.equalTo(true)
        );
    }

    void removeUnmodifiedClasses(Set<String> inputAsmNames) {
        inputAsmNames.remove(INPUT_ASM_VERSIONIZED);
        inputAsmNames.remove(INPUT_ASM_TEST_INNER_CLASS);
    }

    private void compile(Path dirWithInputClassFiles) {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        StandardJavaFileManager fileManager = compiler.getStandardFileManager(
                null,
                null,
                null
        );
        Collection<Path> pathsToCompile = new Walk(SRC)
                .includes(GLOB_JAVA_FILES);
        List<File> filesToCompile = pathsToCompile.stream().map(Path::toFile).collect(Collectors.toList());
        Iterable<? extends JavaFileObject> compilationUnits = fileManager.getJavaFileObjectsFromFiles(filesToCompile);
        List<String> javacOptions = new ArrayList<>();
        javacOptions.add("-d" );
        javacOptions.add(dirWithInputClassFiles.toAbsolutePath().toString());
        compiler.getTask(
                null,
                fileManager,
                null,
                javacOptions,
                null,
                compilationUnits
        ).call();
    }

    private Set<String> getAllDefaultClassChecks() {
        Set<String> allDefaultsClassChecks = new HashSet<>();

        allDefaultsClassChecks.add(SUPER_CLASS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(INTERFACES_DEFAULT_CHECK);
        allDefaultsClassChecks.add(MODULE_DEFAULT_CHECK);
        allDefaultsClassChecks.add(OUTER_CLASS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(VISIBLE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(INVISIBLE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(VISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(INVISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(ATTRS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(INNER_CLASSES_DEFAULT_CHECK);
        allDefaultsClassChecks.add(NEST_HOST_CLASS_DEFAULT_CHECK);
        allDefaultsClassChecks.add(NEST_MEMBERS_DEFAULT_CHECK);

        return allDefaultsClassChecks;
    }

    private Set<String> getAllDefaultFieldChecks() {
        Set<String> allDefaultFieldChecks = new HashSet<>();

        allDefaultFieldChecks.add(VISIBLE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultFieldChecks.add(INVISIBLE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultFieldChecks.add(VISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultFieldChecks.add(INVISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK);
        allDefaultFieldChecks.add(ATTRS_DEFAULT_CHECK);

        return allDefaultFieldChecks;
    }

    /**
     * @return ClassNode for creating custom checks
     */
    private ClassNode doDefaultClassChecks(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath,
            Set<String> defaultClassChecks,
            boolean isVersionized
    ) throws IOException {

        String outputAsmName = isVersionized ? HASH + ASM_SLASH + inputAsmName : inputAsmName;

        Optional<Path> outputPathOptional = outputPaths
                .stream()
                .filter(path -> ModifyBytecodeMojo.pathToAsmName(path, outputDirPath).equals(outputAsmName))
                .findFirst();
        MatcherAssert.assertThat(
                "Can't find output .class file with name in ASM format: " + inputAsmName,
                outputPathOptional.isPresent(),
                Matchers.equalTo(true)
        );
        outputPaths.remove(outputPathOptional.get());

        ClassNode classNode = getClassNode(outputAsmName, outputDirPath);
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong name in ASM format",
                classNode.name,
                Matchers.equalTo(outputAsmName)
        );

        if (defaultClassChecks.contains(SUPER_CLASS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has wrong superclass",
                    classNode.superName,
                    Matchers.equalTo(ASM_OBJECT)
            );
        }

        if (defaultClassChecks.contains(INNER_CLASSES_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has irrelevant inner classes. "
                            + classNode.innerClasses,
                    classNode.innerClasses.isEmpty(),
                    Matchers.equalTo(true)
            );
        }

        if (defaultClassChecks.contains(INTERFACES_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has irrelevant interfaces. "
                            + classNode.interfaces,
                    classNode.interfaces.isEmpty(),
                    Matchers.equalTo(true)
            );
        }

        if (defaultClassChecks.contains(MODULE_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has wrong module",
                    classNode.module,
                    Matchers.equalTo(null)
            );
        }

        if (defaultClassChecks.contains(OUTER_CLASS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has outer class",
                    classNode.outerClass,
                    Matchers.equalTo(null)
            );
        }

        doDefaultClassChecksAnnotations(
                classNode,
                defaultClassChecks,
                inputAsmName,
                isVersionized
        );

        if (defaultClassChecks.contains(ATTRS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has irrelevant attributes",
                    classNode.attrs,
                    Matchers.equalTo(null)
            );
        }

        if (defaultClassChecks.contains(NEST_HOST_CLASS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has nest host class",
                    classNode.nestHostClass,
                    Matchers.equalTo(null)
            );
            String sourceFile = outputAsmName.substring(
                    outputAsmName.lastIndexOf(ASM_SLASH) + 1) + EXTENSION_JAVA;
            MatcherAssert.assertThat(
                    "The source .java file for " + inputAsmName + " has wrong name inside bytecode",
                    classNode.sourceFile,
                    Matchers.equalTo(sourceFile)
            );
        }

        if (defaultClassChecks.contains(NEST_MEMBERS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has irrelevant nest members",
                    classNode.nestMembers,
                    Matchers.equalTo(null)
            );
        }

        return classNode;
    }

    /**
     * The definitions of visible and invisible annotations could be found in the ASM library doc
     */
    private void doDefaultClassChecksAnnotations(
            ClassNode classNode,
            Set<String> defaultClassChecks,
            String inputAsmName,
            boolean isVersionized
    ) {
        if (defaultClassChecks.contains(VISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has irrelevant visible type annotations",
                    classNode.visibleTypeAnnotations,
                    Matchers.equalTo(null)
            );
        }
        if (defaultClassChecks.contains(INVISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has irrelevant invisible type annotations",
                    classNode.invisibleTypeAnnotations,
                    Matchers.equalTo(null)
            );
        }

        if (defaultClassChecks.contains(VISIBLE_ANNOTATIONS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has irrelevant visible annotations",
                    classNode.visibleAnnotations,
                    Matchers.equalTo(null)
            );
        }
        if (defaultClassChecks.contains(INVISIBLE_ANNOTATIONS_DEFAULT_CHECK)) {
            if (isVersionized) {
                MatcherAssert.assertThat(
                        "The output class for " + inputAsmName + " has wrong number of invisible annotations. "
                                + classNode.invisibleAnnotations,
                        classNode.invisibleAnnotations.size(),
                        Matchers.equalTo(1)
                );
                MatcherAssert.assertThat(
                        "The output class for " + inputAsmName + " doesn't have Versionized annotation",
                        classNode.invisibleAnnotations
                                .stream()
                                .map(a -> a.desc)
                                .anyMatch(ASM_DESC_VERSIONIZED::equals),
                        Matchers.equalTo(true)
                );
            } else {
                MatcherAssert.assertThat(
                        "The output class for " + inputAsmName + " has irrelevant invisible annotations",
                        classNode.invisibleAnnotations,
                        Matchers.equalTo(null)
                );
            }
        }
    }

    private void doDefaultFieldChecks(
            FieldNode fieldNode,
            Set<String> defaultFieldChecks,
            String inputAsmName
    ) {

        if (defaultFieldChecks.contains(VISIBLE_ANNOTATIONS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has field " + fieldNode.desc
                            + " with irrelevant visible annotations: "
                            + fieldNode.visibleAnnotations,
                    fieldNode.visibleAnnotations,
                    Matchers.equalTo(null)
            );
        }

        if (defaultFieldChecks.contains(INVISIBLE_ANNOTATIONS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has field " + fieldNode.desc
                            + " with irrelevant invisible annotations: " + fieldNode.visibleAnnotations
                            + fieldNode.invisibleAnnotations,
                    fieldNode.invisibleAnnotations,
                    Matchers.equalTo(null)
            );
        }

        if (defaultFieldChecks.contains(VISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has field " + fieldNode.desc
                            + " with irrelevant visible type annotations: "
                            + fieldNode.visibleAnnotations,
                    fieldNode.visibleTypeAnnotations,
                    Matchers.equalTo(null)
            );
        }

        if (defaultFieldChecks.contains(INVISIBLE_TYPE_ANNOTATIONS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has field " + fieldNode.desc
                            + " with irrelevant invisible type annotations: "
                            + fieldNode.invisibleAnnotations,
                    fieldNode.invisibleTypeAnnotations,
                    Matchers.equalTo(null)
            );
        }

        if (defaultFieldChecks.contains(ATTRS_DEFAULT_CHECK)) {
            MatcherAssert.assertThat(
                    "The output class for " + inputAsmName + " has field " + fieldNode.desc
                            + " with irrelevant attributes: ",
                    fieldNode.attrs,
                    Matchers.equalTo(null)
            );
        }

    }

    private ClassNode getClassNode(String asmName, Path dir) throws IOException {
        Path path = asmNameToPath(asmName, dir);
        ClassReader classReader = new ClassReader(Files.readAllBytes(path));
        ClassNode classNode = new ClassNode();
        classReader.accept(classNode, 0);
        return classNode;
    }

    private Path asmNameToPath(String asmName, Path dir) {
        return dir.resolve(asmName + ModifyBytecodeMojo.EXTENSION_CLASS);
    }

    private void checkFileA(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {

        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                true
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of fields. "
                        + classNode.fields,
                classNode.fields.size(),
                Matchers.equalTo(2)
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + "doesn't have field " + ASM_DESC_B,
                classNode.fields.stream().anyMatch(f -> f.desc.equals(ASM_DESC_B)),
                Matchers.equalTo(true)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + "doesn't have field " + ASM_DESC_C,
                classNode.fields.stream().anyMatch(f -> f.desc.equals(ASM_DESC_C)),
                Matchers.equalTo(true)
        );

        classNode.fields.forEach(field -> doDefaultFieldChecks(
                field, getAllDefaultFieldChecks(),
                inputAsmName
        ));

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(1)
        );
    }

    private void checkFileB(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {

        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                true
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of fields. "
                        + classNode.fields,
                classNode.fields.size(),
                Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + "doesn't have the field",
                classNode.fields.get(0).desc,
                Matchers.equalTo(ASM_DESC_C)
        );

        doDefaultFieldChecks(classNode.fields.get(0), getAllDefaultFieldChecks(), inputAsmName);

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(1)
        );
    }

    private void checkFileC(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {

        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                false
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of fields. ",
                classNode.fields.size(),
                Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + "doesn't have the field",
                classNode.fields.get(0).desc,
                Matchers.equalTo(ASM_DESC_A)
        );


        classNode.fields.forEach(field -> doDefaultFieldChecks(
                field, getAllDefaultFieldChecks(),
                inputAsmName
        ));

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(1)
        );
    }

    private void checkFileInterfaceUsage(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {

        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                true
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of fields. ",
                classNode.fields.size(),
                Matchers.equalTo(0)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(0)
        );
    }

    private void checkFileAnnotationWithVersionized(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {

        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        defaultClassChecks.remove(INTERFACES_DEFAULT_CHECK);
        defaultClassChecks.remove(VISIBLE_ANNOTATIONS_DEFAULT_CHECK);

        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                true
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has irrelevant number of interfaces. "
                        + classNode.interfaces,
                classNode.interfaces.size(),
                Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " doesn't have interface",
                classNode.interfaces.get(0),
                Matchers.equalTo(ASM_DESC_INTERFACE_ANNOTATION)
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has irrelevant number of visible annotation. "
                        + classNode.interfaces,
                classNode.visibleAnnotations.size(),
                Matchers.equalTo(2)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " doesn't have annotation "
                        + ASM_DESC_ANNOTATION_TARGET,
                classNode.visibleAnnotations.stream().anyMatch(a -> a.desc.equals(ASM_DESC_ANNOTATION_TARGET)),
                Matchers.equalTo(true)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " doesn't have annotation "
                        + ASM_DESC_ANNOTATION_RETENTION,
                classNode.visibleAnnotations.stream().anyMatch(a -> a.desc.equals(ASM_DESC_ANNOTATION_RETENTION)),
                Matchers.equalTo(true)
        );

        classNode.fields.forEach(field -> doDefaultFieldChecks(
                field, getAllDefaultFieldChecks(),
                inputAsmName
        ));

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(0)
        );
    }

    /**
     * Host nested class ({@link ModifyBytecodeMojoTest#INPUT_ASM_TEST_INNER_CLASS}) doesn't contain annotation
     * {@code Versionized} and doesn't contain any usage of any classes which is annotated by {@code Versionized} except
     * the inner class implements interface which is annotated by {@code Versionized}
     */
    private void checkFileInnerClass(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {

        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        defaultClassChecks.remove(NEST_HOST_CLASS_DEFAULT_CHECK);
        defaultClassChecks.remove(INTERFACES_DEFAULT_CHECK);
        defaultClassChecks.remove(INNER_CLASSES_DEFAULT_CHECK);

        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                false
        );

        String nestHostClass = inputAsmName.substring(0, inputAsmName.lastIndexOf('$'));
        MatcherAssert.assertThat(
                "Nest host class for " + inputAsmName + " has wrong name inside bytecode",
                classNode.nestHostClass,
                Matchers.equalTo(nestHostClass)
        );

        String sourceFile = inputAsmName.substring(
                inputAsmName.lastIndexOf(ASM_SLASH) + 1,
                inputAsmName.lastIndexOf('$')) + EXTENSION_JAVA;
        MatcherAssert.assertThat(
                "The source .java file for " + inputAsmName + " has wrong name inside bytecode",
                classNode.sourceFile,
                Matchers.equalTo(sourceFile)
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of fields. ",
                classNode.fields.size(),
                Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(1)
        );
    }

    /**
     * This is a check for file {@link ModifyBytecodeMojoTest#INPUT_ASM_TEST_STATIC_NESTED_CLASS}
     * <br>
     * Check for the nested class is here
     * {@link ModifyBytecodeMojoTest#checkFileStaticNestedClass(String, Collection, Path)}
     * <br>
     * This host nested class doesn't contain annotation {@code Versionized} and doesn't contain any usage of any
     * classes annotated by {@code Versionized} except the inner class
     */
    private void checkFileTestStaticNestedClass(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {
        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        defaultClassChecks.remove(INNER_CLASSES_DEFAULT_CHECK);
        defaultClassChecks.remove(NEST_MEMBERS_DEFAULT_CHECK);

        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                false
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of inner classes. "
                        + classNode.innerClasses,
                classNode.innerClasses.size(),
                Matchers.equalTo(1)
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " doesn't have inner class",
                classNode.innerClasses.get(0).name,
                Matchers.equalTo(HASH + ASM_SLASH + INPUT_ASM_STATIC_NESTED_CLASS));
    }

    /**
     * The information about this test case could be found in the comment before the method
     * {@link ModifyBytecodeMojoTest#checkFileTestStaticNestedClass}
     */
    private void checkFileStaticNestedClass(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {

        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        defaultClassChecks.remove(NEST_HOST_CLASS_DEFAULT_CHECK);
        defaultClassChecks.remove(INNER_CLASSES_DEFAULT_CHECK);

        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                true
        );

        String nestHostClass = inputAsmName.substring(0, inputAsmName.lastIndexOf('$'));
        MatcherAssert.assertThat(
                "Nest host class for " + inputAsmName + " has wrong name inside bytecode",
                classNode.nestHostClass,
                Matchers.equalTo(nestHostClass)
        );

        String sourceFile = inputAsmName.substring(
                inputAsmName.lastIndexOf(ASM_SLASH) + 1,
                inputAsmName.lastIndexOf('$')) + EXTENSION_JAVA;
        MatcherAssert.assertThat(
                "The source .java file for " + inputAsmName + " has wrong name inside bytecode",
                classNode.sourceFile,
                Matchers.equalTo(sourceFile)
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of fields. ",
                classNode.fields.size(),
                Matchers.equalTo(0)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(1)
        );
    }

    /**
     * This is a check for file {@link ModifyBytecodeMojoTest#INPUT_ASM_TEST_INNER_CLASS_2}.
     * <br>
     * Check for the nested class is here
     * {@link ModifyBytecodeMojoTest#checkFileInnerClass2(String, Collection, Path)}
     * <br>
     * This host nested class is annotated by {@code Versionized} and the inner class doesn't contain any usage of any
     * class annotated by {@code Versionized}
     */
    private void checkFileTestInnerClass2(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {
        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        defaultClassChecks.remove(INNER_CLASSES_DEFAULT_CHECK);
        defaultClassChecks.remove(NEST_MEMBERS_DEFAULT_CHECK);

        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                true
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of inner classes. "
                        + classNode.innerClasses,
                classNode.innerClasses.size(),
                Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " doesn't have the inner class",
                classNode.innerClasses.get(0).name,
                Matchers.equalTo(INPUT_ASM_INNER_CLASS_2)
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of nested members. "
                        + classNode.nestMembers,
                classNode.nestMembers.size(),
                Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " doesn't have the nested the nested member",
                classNode.nestMembers.get(0),
                Matchers.equalTo(INPUT_ASM_INNER_CLASS_2)
        );
    }

    /**
     * The information about this test case could be found in the comment before the method
     * {@link ModifyBytecodeMojoTest#checkFileTestInnerClass2}
     */
    private void checkFileInnerClass2(
            String inputAsmName,
            Collection<Path> outputPaths,
            Path outputDirPath
    ) throws IOException {
        Set<String> defaultClassChecks = getAllDefaultClassChecks();
        defaultClassChecks.remove(NEST_HOST_CLASS_DEFAULT_CHECK);
        defaultClassChecks.remove(INTERFACES_DEFAULT_CHECK);
        defaultClassChecks.remove(INNER_CLASSES_DEFAULT_CHECK);

        ClassNode classNode = doDefaultClassChecks(
                inputAsmName,
                outputPaths,
                outputDirPath,
                defaultClassChecks,
                false
        );

        String nestHostClass = HASH + ASM_SLASH + inputAsmName.substring(0, inputAsmName.lastIndexOf('$'));
        MatcherAssert.assertThat(
                "Nest host class for " + inputAsmName + " has wrong name inside bytecode",
                classNode.nestHostClass,
                Matchers.equalTo(nestHostClass)
        );

        String sourceFile = inputAsmName.substring(
                inputAsmName.lastIndexOf(ASM_SLASH) + 1,
                inputAsmName.lastIndexOf('$')) + EXTENSION_JAVA;
        MatcherAssert.assertThat(
                "The source .java file for " + inputAsmName + " has wrong name inside bytecode",
                classNode.sourceFile,
                Matchers.equalTo(sourceFile)
        );

        MatcherAssert.assertThat(
                "The output class for " + inputAsmName + " has wrong number of fields. ",
                classNode.fields.size(),
                Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
                "The output class for " + inputAsmName
                        + " has irrelevant number of methods/constructors. " + classNode.methods,
                classNode.methods.size(),
                Matchers.equalTo(1)
        );
    }
}