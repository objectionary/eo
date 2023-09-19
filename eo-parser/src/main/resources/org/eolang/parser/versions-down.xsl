<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" id="default-empty-versions"
                version="2.0">
  <!--
  Here we add empty "ver" attribute for all objects, which "base" starts with "org.eolang."
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