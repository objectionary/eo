<?xml version="1.0" encoding="UTF-8"?>
<!--
The MIT License (MIT)

Copyright (c) 2016-2023 Objectionary.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="move-versions-deeper" version="2.0">
  <!--
  Here we move "ver" attribute from the top object deeper to pre-last one (before <o base="Q"/>)

  1. The reason of transformation - move version to pre-last object ("org"). Then on "transpile"
     step "to-java.xsl" will add extra {@code attr("some-hash").get()} java code. It will allow us
     to claim version as separated "package" object.
     More details here (solution, point 3): https://github.com/objectionary/eo/issues/2503
  2. The one application of transformation moves version to one level deeper.
  3. Since we need to move the version from the top object to the deepest object, transformation
     must be applied many times until xml is not changed after applying anymore. The best way to
     do it - place the transformation in the end, after all other transformations are done.

  The result of applying transformation several times:
  <o ver="123">            <o>
    <o>                      <o>
      <o>                      <o>
        <o base="org">           <o base="org" ver="123">
          <o base="Q"/>    =>      <base="Q"/>
        </o>                     </o>
      </o>                     </o>
    </o>                     </o>
  </o>                     </o>

  Or:

  <o>                      <o>
    <o ver="123">            <o>
      <o>                      <o>
        <o base="org">           <o base="org" ver="123">
          <o base="Q"/>    =>      <base="Q"/>
        </o>                     </o>
      </o>                     </o>
    </o>                     </o>
  </o>                     </o>
  -->
  <xsl:output encoding="UTF-8" method="xml"/>
  <xsl:template match="//o[starts-with(@base,'.') and @ver and parent::o[not(starts-with(@base,'.')) or not(@ver)] and o[position()=1 and starts-with(@base,'.')]]">
    <xsl:element name="{name()}">
      <xsl:for-each select="@*[name()!='ver']">
        <xsl:attribute name="{name()}">
          <xsl:value-of select="."/>
        </xsl:attribute>
      </xsl:for-each>
      <xsl:element name="o">
        <xsl:for-each select="o[position()=1]/@*[name()!='ver']">
          <xsl:attribute name="{name()}">
            <xsl:value-of select="."/>
          </xsl:attribute>
        </xsl:for-each>
        <xsl:attribute name="ver">
          <xsl:value-of select="@ver"/>
        </xsl:attribute>
        <xsl:copy-of select="./o[position()=1]/*"/>
      </xsl:element>
      <xsl:copy-of select="./o[position()!=1]"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
