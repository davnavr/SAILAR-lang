namespace SAILARSharp.Interop;

using System.Runtime.InteropServices;
using System.Text;

internal unsafe static class Identifier {
    internal readonly struct Opaque { }

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_identifier_from_utf16", ExactSpelling = true)]
    private static extern Opaque* Create(char* contents, nuint count, out Error.Opaque* error);

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_identifier", ExactSpelling = true)]
    internal static extern void Dispose(Opaque* identifier);
    
    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_identifier_contents", ExactSpelling = true)]
    private static extern byte* GetContents(Opaque* identifier, out nuint length);

    /// <summary>
    /// Retrieves the contents of the <paramref name="identifier"/> as a <see cref="string"/>, and disposes the
    /// <paramref name="identifier"/>.
    /// </summary>
    internal static string IntoString(Opaque* identifier) {
        if (identifier == null) {
            return string.Empty;
        }

        nuint length;
        byte* contents = GetContents(identifier, out length);
        
        Dispose(identifier);

        return length == 0 ? string.Empty : Encoding.UTF8.GetString(contents, (int)length);
    }

    /// <summary>Converts a .NET <see cref="string"/> into a SAILAR identifier string.</summary>
    /// <exception cref="ErrorMessageException">Thrown if the <paramref name="identifier"/> was invalid.</exception>
    internal static Opaque* FromString(string? identifier) {
        fixed(char* contents = identifier) {
            Error.Opaque* error;
            Opaque* id = Create(contents, identifier == null ? 0 : (nuint)identifier.Length, out error);
            Error.Throw(error);
            return id;
        }
    }
}
