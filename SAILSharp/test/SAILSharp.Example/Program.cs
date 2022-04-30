namespace SAILSharp.Example {
    using SAILSharp;

    public class Program {
        public static void Main() {
            using var module = new ModuleDefinition("MyModule", new int[] { 1, 1, 0 });
            unsafe { Interop.SAILAR.DumpModuleDebug(module.GetRawReference()); }
        }
    }
}
