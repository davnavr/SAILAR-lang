namespace SAILSharp.Reader {
    using System;
    using System.Diagnostics;
    using SAILSharp;
    using SAILSharp.Interop;

    public unsafe sealed class ModuleReader : IDisposable {
        private OpaqueModuleReader* reader;
        private OpaqueBuffer* buffer = default;
        private ModuleFormat? format = null;

        private readonly bool disposeMemoryBuffer = false;
        private bool disposed = false;

        public ModuleReader(OpaqueBuffer* buffer, bool readerDisposesBuffer) {
            this.buffer = buffer;
            disposeMemoryBuffer = readerDisposesBuffer;
            reader = SAILAR.CreateModuleReaderFromBuffer(buffer);
        }

        public ModuleReader(Interop.Buffer buffer, bool readerDisposesBuffer) : this(buffer.Reference, readerDisposesBuffer) {}

        public ModuleReader(byte[] bytes) : this(new Interop.Buffer(bytes), true) { }

        /// <summary>
        /// Gets or reads the module's format version and integer size.
        /// </summary>
        /// <exception cref="ErrorException">Thrown if an error occured while reading the module format.</exception>
        public ModuleFormat GetModuleFormat() {
            if (format == null) {
                OpaqueError* error;
                format = new ModuleFormat(SAILAR.ReadModuleFormat(reader, &error));
            }

            return format;
        }

        public void Dispose() {
            if (!disposed) {
                GC.SuppressFinalize(this);

                SAILAR.DisposeModuleReader(reader);
                reader = null;

                if (disposeMemoryBuffer) {
                    new Interop.Buffer(buffer).Dispose();
                    buffer = default;
                }

                disposed = true;
            }
        }

        ~ModuleReader() {
            Dispose();
        }
    }
}
