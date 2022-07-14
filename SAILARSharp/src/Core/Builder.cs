namespace SAILARSharp.Core;

using SAILARSharp.Interop;
using System;
using System.Runtime.InteropServices;
using System.Threading;

/// <summary>
/// <p>Builds the list of records in a SAILAR module.</p>
/// <p>This class is thread safe.</p>
/// </summary>
public unsafe sealed class Builder : IDisposable {
    internal readonly struct Opaque { }

    private readonly object locker = new();

    private Opaque* builder;
    
    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_builder_create", ExactSpelling = true)]
    private static extern Opaque* CreateEmpty();
    
    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_builder_dispose", ExactSpelling = true)]
    private static extern void Dispose(Opaque* builder);
    
    [DllImport("SAILARCore", CallingConvention = CallingConvention.Cdecl, EntryPoint = "sailar_builder_write_to_path", ExactSpelling = true)]
    private static extern void WriteToPath(Opaque* builder, Path.Opaque* path, out Error.Opaque* error);

    private Builder(Opaque* builder) {
        this.builder = builder;
    }

    public Builder Create() {
        return new(CreateEmpty());
    }

    private void CheckIfDisposed() {
        if (builder == null) {
            throw new ObjectDisposedException(GetType().FullName);
        }
    }

    /// <summary>Attempts to write the contents of the builder to the specified file <paramref name="path"/></summary>
    public void WriteTo(string path) {
        var destination = Path.FromString(path);

        lock (locker) {
            CheckIfDisposed();
            Error.Opaque* error;
            WriteToPath(builder, destination, out error);
            Error.Throw(error);
        }

        Path.Dispose(destination);
    }

    private void Dispose(bool disposing) {
        try {
            if (disposing) {
                Monitor.Enter(locker);
            }

            if (builder != null) {
                Dispose(builder);
                builder = null;
            }
        } finally {
            if (disposing) {
                Monitor.Exit(locker);
            }
        }
    }

    public void Dispose() {
        Dispose(true);
        GC.SuppressFinalize(this);
    }

    ~Builder() => Dispose(false);
}
