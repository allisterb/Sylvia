namespace Sylvia.Data;

using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Xml;
using System.Xml.Serialization;

public class GretlFile
{
    public static void ToCsvFile(string inputFile, string outputFile)
    {        
        XmlReaderSettings settings = new XmlReaderSettings()
        {
            DtdProcessing = DtdProcessing.Parse,
            ValidationType = ValidationType.None
        };
        using var stream = XmlReader.Create(new GZipStream(new FileStream(inputFile, FileMode.Open), CompressionMode.Decompress), settings);             
        XmlSerializer serializer = new XmlSerializer(typeof(gretldata));
        gretldata gdt = (gretldata)serializer.Deserialize(stream);
        stream.Close();
        using StreamWriter writer = new StreamWriter(outputFile);        
        var header = string.Join(",", gdt.variables.variable.Select(v => v.name));
        writer.WriteLine(header);
        foreach (var obs in gdt.observations.obs)
        {
            var values = obs.Split([' '], options:StringSplitOptions.RemoveEmptyEntries);
            var line = string.Join(",", values);
            writer.WriteLine(line);
        }                
        writer.Close();
    }
}


// NOTE: Generated code may require at least .NET Framework 4.5 or .NET Core/Standard 2.0.
/// <remarks/>
[System.SerializableAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
[System.Xml.Serialization.XmlRootAttribute(Namespace = "", IsNullable = false)]
public partial class gretldata
{

    private string descriptionField;

    private gretldataVariables variablesField;

    private gretldataObservations observationsField;

    private decimal versionField;

    private string nameField;

    private byte frequencyField;

    private ushort startobsField;

    private ushort endobsField;

    private string typeField;

    /// <remarks/>
    public string description
    {
        get
        {
            return this.descriptionField;
        }
        set
        {
            this.descriptionField = value;
        }
    }

    /// <remarks/>
    public gretldataVariables variables
    {
        get
        {
            return this.variablesField;
        }
        set
        {
            this.variablesField = value;
        }
    }

    /// <remarks/>
    public gretldataObservations observations
    {
        get
        {
            return this.observationsField;
        }
        set
        {
            this.observationsField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public decimal version
    {
        get
        {
            return this.versionField;
        }
        set
        {
            this.versionField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public string name
    {
        get
        {
            return this.nameField;
        }
        set
        {
            this.nameField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public byte frequency
    {
        get
        {
            return this.frequencyField;
        }
        set
        {
            this.frequencyField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public ushort startobs
    {
        get
        {
            return this.startobsField;
        }
        set
        {
            this.startobsField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public ushort endobs
    {
        get
        {
            return this.endobsField;
        }
        set
        {
            this.endobsField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public string type
    {
        get
        {
            return this.typeField;
        }
        set
        {
            this.typeField = value;
        }
    }
}

/// <remarks/>
[System.SerializableAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
public partial class gretldataVariables
{

    private gretldataVariablesVariable[] variableField;

    private int countField;

    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("variable")]
    public gretldataVariablesVariable[] variable
    {
        get
        {
            return this.variableField;
        }
        set
        {
            this.variableField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public int count
    {
        get
        {
            return this.countField;
        }
        set
        {
            this.countField = value;
        }
    }
}

/// <remarks/>
[System.SerializableAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
public partial class gretldataVariablesVariable
{

    private string nameField;

    private string labelField;

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public string name
    {
        get
        {
            return this.nameField;
        }
        set
        {
            this.nameField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public string label
    {
        get
        {
            return this.labelField;
        }
        set
        {
            this.labelField = value;
        }
    }
}

/// <remarks/>
[System.SerializableAttribute()]
[System.ComponentModel.DesignerCategoryAttribute("code")]
[System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true)]
public partial class gretldataObservations
{

    private string[] obsField;

    private int countField;

    private bool labelsField;

    /// <remarks/>
    [System.Xml.Serialization.XmlElementAttribute("obs")]
    public string[] obs
    {
        get
        {
            return this.obsField;
        }
        set
        {
            this.obsField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public int count
    {
        get
        {
            return this.countField;
        }
        set
        {
            this.countField = value;
        }
    }

    /// <remarks/>
    [System.Xml.Serialization.XmlAttributeAttribute()]
    public bool labels
    {
        get
        {
            return this.labelsField;
        }
        set
        {
            this.labelsField = value;
        }
    }
}

