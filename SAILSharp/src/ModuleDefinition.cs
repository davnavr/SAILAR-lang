namespace SAILSharp {
    using System;
    using System.Collections.Generic;
    using System.Collections.Immutable;
    using SAILSharp.Interop;

    public unsafe sealed class ModuleDefinition : IDisposable {
        private bool disposed = false;

        private readonly SAILAR.OpaqueModule* module;
        private readonly string cachedModuleName;
        private readonly ImmutableArray<UIntPtr> cachedModuleVersion;

        public ModuleDefinition(string name, IList<int> version) {
            cachedModuleName = name;

            ImmutableArray<UIntPtr>.Builder versionNumbersBuilder = ImmutableArray.CreateBuilder<UIntPtr>(version.Count);
            foreach (int number in version) {
                versionNumbersBuilder.Add((UIntPtr)number);
            }

            cachedModuleVersion = versionNumbersBuilder.Capacity != versionNumbersBuilder.Count ? cachedModuleVersion = versionNumbersBuilder.ToImmutable() : versionNumbersBuilder.MoveToImmutable();

            using var moduleNameIdentifier = new Identifier(name);
            fixed (UIntPtr* versionNumbers = cachedModuleVersion.AsSpan()) {
                module = SAILAR.CreateModule(moduleNameIdentifier.identifier, versionNumbers, (UIntPtr)cachedModuleVersion.Length);
            }
        }

        public string Name => cachedModuleName;

        public ImmutableArray<UIntPtr> Version => cachedModuleVersion;

        public SAILAR.OpaqueModule* GetRawReference() => module;

        public void Dispose() {
            if (disposed) {
                return;
            }

            SAILAR.DisposeModule(module);
            disposed = true;
        }

        ~ModuleDefinition() => Dispose();
    }
}
