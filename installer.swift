#!/usr/bin/env swift

import Foundation

// MARK: - Installer for PSYOP Website Setup.hs
// This script checks for required dependencies and runs the Haskell setup

// ANSI color codes for terminal output
struct Colors {
    static let reset = "\u{001B}[0m"
    static let red = "\u{001B}[31m"
    static let green = "\u{001B}[32m"
    static let yellow = "\u{001B}[33m"
    static let blue = "\u{001B}[34m"
    static let magenta = "\u{001B}[35m"
    static let cyan = "\u{001B}[36m"
    static let white = "\u{001B}[37m"
    static let bold = "\u{001B}[1m"
}

// Logging functions with colors
func logInfo(_ message: String) {
    print("\(Colors.blue)[INFO]\(Colors.reset) \(message)")
}

func logSuccess(_ message: String) {
    print("\(Colors.green)[SUCCESS]\(Colors.reset) \(message)")
}

func logWarning(_ message: String) {
    print("\(Colors.yellow)[WARNING]\(Colors.reset) \(message)")
}

func logError(_ message: String) {
    print("\(Colors.red)[ERROR]\(Colors.reset) \(message)")
}

func logBold(_ message: String) {
    print("\(Colors.bold)\(message)\(Colors.reset)")
}

// MARK: - Dependency Checker
class DependencyChecker {
    
    // Check if a command exists in PATH
    func commandExists(_ command: String) -> Bool {
        let task = Process()
        task.launchPath = "/usr/bin/which"
        task.arguments = [command]
        
        let pipe = Pipe()
        task.standardOutput = pipe
        task.standardError = pipe
        
        do {
            try task.run()
            task.waitUntilExit()
            return task.terminationStatus == 0
        } catch {
            return false
        }
    }
    
    // Check if Homebrew is installed
    func isHomebrewInstalled() -> Bool {
        return commandExists("brew")
    }
    
    // Check if Xcode Command Line Tools are installed
    func areXcodeToolsInstalled() -> Bool {
        return commandExists("xcode-select") && commandExists("clang")
    }
    
    // Check if Haskell Stack is installed
    func isHaskellStackInstalled() -> Bool {
        return commandExists("stack")
    }
    
    // Check if Git is installed
    func isGitInstalled() -> Bool {
        return commandExists("git")
    }
    
    // Check if Node.js is installed (for potential frontend tools)
    func isNodeInstalled() -> Bool {
        return commandExists("node")
    }
    
    // Check if Python3 is installed
    func isPythonInstalled() -> Bool {
        return commandExists("python3")
    }
    
    // Check system requirements
    func checkSystemRequirements() -> [String: Bool] {
        logInfo("Checking system requirements...")
        
        let requirements: [String: Bool] = [
            "Xcode Command Line Tools": areXcodeToolsInstalled(),
            "Homebrew": isHomebrewInstalled(),
            "Git": isGitInstalled(),
            "Haskell Stack": isHaskellStackInstalled(),
            "Python3": isPythonInstalled(),
            "Node.js": isNodeInstalled()
        ]
        
        // Display results
        for (requirement, installed) in requirements {
            let status = installed ? "✅" : "❌"
            let message = installed ? "Installed" : "Not installed"
            print("\(status) \(requirement): \(message)")
        }
        
        return requirements
    }
}

// MARK: - Dependency Installer
class DependencyInstaller {
    
    // Install Xcode Command Line Tools
    func installXcodeTools() -> Bool {
        logInfo("Installing Xcode Command Line Tools...")
        
        if !runCommand("xcode-select --install") {
            logError("Failed to install Xcode Command Line Tools")
            logWarning("Please install manually from: https://developer.apple.com/xcode/")
            return false
        }
        
        logSuccess("Xcode Command Line Tools installation initiated")
        logWarning("Please complete the installation in the popup window and restart this script")
        return false // Return false to indicate manual intervention needed
    }
    
    // Install Homebrew
    func installHomebrew() -> Bool {
        logInfo("Installing Homebrew...")
        
        let installScript = "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
        
        if !runCommand(installScript) {
            logError("Failed to install Homebrew")
            return false
        }
        
        // Add Homebrew to PATH if needed
        let shellConfig = getShellConfigFile()
        let homebrewPath = "export PATH=\"/opt/homebrew/bin:$PATH\""
        
        if !runCommand("echo '\(homebrewPath)' >> \(shellConfig)") {
            logWarning("Failed to add Homebrew to PATH. Please add manually to \(shellConfig)")
        }
        
        logSuccess("Homebrew installed successfully")
        return true
    }
    
    // Install Haskell Stack
    func installHaskellStack() -> Bool {
        logInfo("Installing Haskell Stack...")
        
        if !runCommand("curl -sSL https://get.haskellstack.org/ | sh") {
            logError("Failed to install Haskell Stack")
            return false
        }
        
        logSuccess("Haskell Stack installed successfully")
        return true
    }
    
    // Install Git
    func installGit() -> Bool {
        logInfo("Installing Git...")
        
        if !runCommand("brew install git") {
            logError("Failed to install Git")
            return false
        }
        
        logSuccess("Git installed successfully")
        return true
    }
    
    // Install Python3
    func installPython3() -> Bool {
        logInfo("Installing Python3...")
        
        if !runCommand("brew install python") {
            logError("Failed to install Python3")
            return false
        }
        
        logSuccess("Python3 installed successfully")
        return true
    }
    
    // Install Node.js
    func installNode() -> Bool {
        logInfo("Installing Node.js...")
        
        if !runCommand("brew install node") {
            logError("Failed to install Node.js")
            return false
        }
        
        logSuccess("Node.js installed successfully")
        return true
    }
    
    // Get the shell configuration file
    func getShellConfigFile() -> String {
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        
        if shell.contains("zsh") {
            return ProcessInfo.processInfo.environment["HOME"]! + "/.zshrc"
        } else if shell.contains("bash") {
            return ProcessInfo.processInfo.environment["HOME"]! + "/.bash_profile"
        } else {
            return ProcessInfo.processInfo.environment["HOME"]! + "/.profile"
        }
    }
    
    // Install all missing dependencies
    func installMissingDependencies(_ requirements: [String: Bool]) -> Bool {
        logInfo("Installing missing dependencies...")
        
        var allInstalled = true
        
        // Install Xcode Tools first if needed
        if !requirements["Xcode Command Line Tools"]! {
            if !installXcodeTools() {
                return false
            }
        }
        
        // Install Homebrew if needed
        if !requirements["Homebrew"]! {
            if !installHomebrew() {
                allInstalled = false
            }
        }
        
        // Install other dependencies using Homebrew
        if requirements["Homebrew"]! {
            if !requirements["Git"]! {
                if !installGit() {
                    allInstalled = false
                }
            }
            
            if !requirements["Python3"]! {
                if !installPython3() {
                    allInstalled = false
                }
            }
            
            if !requirements["Node.js"]! {
                if !installNode() {
                    allInstalled = false
                }
            }
        }
        
        // Install Haskell Stack
        if !requirements["Haskell Stack"]! {
            if !installHaskellStack() {
                allInstalled = false
            }
        }
        
        return allInstalled
    }
}

// MARK: - Setup Runner
class SetupRunner {
    
    // Check if dev-server.hs exists
    func setupFileExists() -> Bool {
        let fileManager = FileManager.default
        return fileManager.fileExists(atPath: "config/dev-server.hs")
    }
    
    // Check if we're in the right directory
    func checkProjectStructure() -> Bool {
        let fileManager = FileManager.default
        let requiredFiles = ["src", "package.yaml", "stack.yaml"]
        
        for file in requiredFiles {
            if !fileManager.fileExists(atPath: file) {
                logError("Required file/directory not found: \(file)")
                return false
            }
        }
        
        return true
    }
    
    // Run the dev-server.hs setup
    func runSetup() -> Bool {
        logInfo("Running development server setup...")
        
        if !setupFileExists() {
            logError("config/dev-server.hs not found in current directory")
            return false
        }
        
        if !checkProjectStructure() {
            logError("Project structure is incomplete")
            return false
        }
        
        // Try to run setup with runhaskell first
        logInfo("Attempting to run setup with runhaskell...")
        if runCommand("runhaskell config/dev-server.hs setup") {
            logSuccess("Development server setup completed successfully")
            return true
        }
        
        // Fallback to stack exec
        logWarning("runhaskell failed, trying with stack...")
        if runCommand("stack exec runhaskell config/dev-server.hs setup") {
            logSuccess("Development server setup completed successfully with stack")
            return true
        }
        
        // Final fallback to ghc
        logWarning("stack failed, trying with ghc...")
        if runCommand("ghc config/dev-server.hs && ./dev-server setup") {
            logSuccess("Development server setup completed successfully with ghc")
            return true
        }
        
        logError("All methods to run dev-server setup failed")
        logInfo("Let's try to diagnose the issue...")
        
        // Diagnostic information
        let (stackVersion, stackOutput) = runCommandWithOutput("stack --version")
        if stackVersion {
            logInfo("Stack version: \(stackOutput)")
        } else {
            logWarning("Stack version check failed: \(stackOutput)")
        }
        
        let (ghcVersion, ghcOutput) = runCommandWithOutput("ghc --version")
        if ghcVersion {
            logInfo("GHC version: \(ghcOutput)")
        } else {
            logWarning("GHC version check failed: \(ghcOutput)")
        }
        
        let (runhaskellVersion, runhaskellOutput) = runCommandWithOutput("runhaskell --version")
        if runhaskellVersion {
            logInfo("runhaskell version: \(runhaskellOutput)")
        } else {
            logWarning("runhaskell version check failed: \(runhaskellOutput)")
        }
        
        return false
    }
}

// MARK: - Utility Functions
func runCommand(_ command: String) -> Bool {
    let task = Process()
    task.launchPath = "/bin/bash"
    task.arguments = ["-c", command]
    
    let pipe = Pipe()
    let errorPipe = Pipe()
    task.standardOutput = pipe
    task.standardError = errorPipe
    
    do {
        try task.run()
        task.waitUntilExit()
        
        let outputData = pipe.fileHandleForReading.readDataToEndOfFile()
        let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
        
        let output = String(data: outputData, encoding: .utf8) ?? ""
        let error = String(data: errorData, encoding: .utf8) ?? ""
        
        if task.terminationStatus != 0 {
            logWarning("Command failed: \(command)")
            if !output.isEmpty { print("Output: \(output)") }
            if !error.isEmpty { print("Error: \(error)") }
            return false
        }
        
        if !output.isEmpty { print("Output: \(output)") }
        return true
    } catch {
        logError("Failed to run command: \(command)")
        logError("Error: \(error)")
        return false
    }
}

func runCommandWithOutput(_ command: String) -> (success: Bool, output: String) {
    let task = Process()
    task.launchPath = "/bin/bash"
    task.arguments = ["-c", command]
    
    let pipe = Pipe()
    let errorPipe = Pipe()
    task.standardOutput = pipe
    task.standardError = errorPipe
    
    do {
        try task.run()
        task.waitUntilExit()
        
        let outputData = pipe.fileHandleForReading.readDataToEndOfFile()
        let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
        
        let output = String(data: outputData, encoding: .utf8) ?? ""
        let error = String(data: errorData, encoding: .utf8) ?? ""
        
        let fullOutput = output + (error.isEmpty ? "" : "\nError: \(error)")
        
        return (task.terminationStatus == 0, fullOutput)
    } catch {
        logError("Failed to run command: \(command)")
        logError("Error: \(error)")
        return (false, "")
    }
}

// MARK: - Main Installation Flow
func main() {
    logBold("🚀 PSYOP Website Installer")
    logBold("==========================")
    print("")
    
    // Check current directory
    let currentDir = FileManager.default.currentDirectoryPath
    logInfo("Current directory: \(currentDir)")
    
    // Check if we're in the right place
    if !currentDir.hasSuffix("psyop.ca") {
        logWarning("Current directory doesn't appear to be the psyop.ca project")
        logWarning("Please run this script from the project root directory")
        print("")
    }
    
    // Initialize components
    let dependencyChecker = DependencyChecker()
    let dependencyInstaller = DependencyInstaller()
    let setupRunner = SetupRunner()
    
    // Check system requirements
    let requirements = dependencyChecker.checkSystemRequirements()
    print("")
    
    // Check if all requirements are met
    let allRequirementsMet = requirements.values.allSatisfy { $0 }
    
    if !allRequirementsMet {
        logWarning("Some dependencies are missing. Installing...")
        print("")
        
        if !dependencyInstaller.installMissingDependencies(requirements) {
            logError("Failed to install some dependencies")
            logWarning("Please install them manually and run this script again")
            print("")
            logInfo("Manual installation commands:")
            print("  - Xcode Tools: xcode-select --install")
            print("  - Homebrew: /bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\"")
            print("  - Haskell Stack: curl -sSL https://get.haskellstack.org/ | sh")
            print("  - Git: brew install git")
            print("  - Python3: brew install python")
            print("  - Node.js: brew install node")
            return
        }
        
        logSuccess("All dependencies installed successfully")
        print("")
    } else {
        logSuccess("All system requirements are met")
        print("")
    }
    
    // Verify dev-server.hs exists
    if !setupRunner.setupFileExists() {
        logError("config/dev-server.hs not found in current directory")
        logError("Please ensure you're running this script from the project root")
        return
    }
    
    // Run the setup
    logInfo("Starting PSYOP website setup...")
    print("")
    
    if setupRunner.runSetup() {
        print("")
        logSuccess("🎉 PSYOP Website setup completed successfully!")
        print("")
        logInfo("Next steps:")
        logInfo("1. Check the status: runhaskell config/dev-server.hs status")
        logInfo("2. Start the development server: runhaskell config/dev-server.hs dev")
        logInfo("3. Visit http://localhost:8080 in your browser")
        print("")
    } else {
        print("")
        logError("❌ PSYOP Website setup failed")
        print("")
        logInfo("Troubleshooting:")
        logInfo("1. Check the error messages above")
        logInfo("2. Ensure all dependencies are properly installed")
        logInfo("3. Try running dev-server setup manually: runhaskell config/dev-server.hs setup")
        print("")
    }
}

// MARK: - Script Execution
// Check if script is being run directly
if CommandLine.arguments.count > 1 {
    // Handle command line arguments
    let command = CommandLine.arguments[1]
    
    switch command {
    case "check":
        let checker = DependencyChecker()
        _ = checker.checkSystemRequirements()
    case "install-deps":
        let checker = DependencyChecker()
        let installer = DependencyInstaller()
        let requirements = checker.checkSystemRequirements()
        _ = installer.installMissingDependencies(requirements)
    case "run-setup":
        let runner = SetupRunner()
        _ = runner.runSetup()
    case "help", "-h", "--help":
        print("PSYOP Website Installer")
        print("")
        print("Usage: swift installer.swift [COMMAND]")
        print("")
        print("Commands:")
        print("  check        Check system requirements")
        print("  install-deps Install missing dependencies")
        print("  run-setup    Run Setup.hs only")
        print("  help         Show this help message")
        print("")
        print("Examples:")
        print("  swift installer.swift           # Full installation")
        print("  swift installer.swift check     # Check requirements only")
        print("  swift installer.swift install-deps # Install dependencies only")
    default:
        print("Unknown command: \(command)")
        print("Use 'swift installer.swift help' for usage information")
    }
} else {
    // Run full installation
    main()
}
