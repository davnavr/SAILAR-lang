namespace SAILARSharp.Reader {
    using System;
    using SAILARSharp;
    using SAILARSharp.Interop;

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

        public ModuleReader(byte[] bytes) : this(Interop.Buffer.From(bytes), true) { }

        public OpaqueModuleReader* Reference => reader;

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
                    SAILAR.DisposeBuffer(buffer);
                    buffer = null;
                }

                disposed = true;
            }
        }

        ~ModuleReader() {
            Dispose();
        }
    }
}
