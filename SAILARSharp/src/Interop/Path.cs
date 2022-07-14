namespace SAILARSharp.Interop;

using System.Runtime.InteropServices;

internal unsafe static class Path {
    internal readonly struct Opaque { }

    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_path_from_utf16", ExactSpelling = true)]
    private static extern Opaque* Create(char* contents, nuint count, out Error.Opaque* error);


    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_dispose_path", ExactSpelling = true)]
    internal static extern void Dispose(Opaque* path);

    internal static Opaque* FromString(string path) {
        fixed (char* characters = path) {
            Error.Opaque* error;
            var p = Create(characters, path == null ? 0 : (nuint)path.Length, out error);
            Error.Throw(error);
            return p;
        }
    }
}
