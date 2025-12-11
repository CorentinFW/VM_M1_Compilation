#!/bin/bash
###############################################################################
# RUN ALL TESTS - Script pour exécuter tous les tests du projet
###############################################################################

echo "═══════════════════════════════════════════════════════════════"
echo "EXÉCUTION DE TOUS LES TESTS"
echo "═══════════════════════════════════════════════════════════════"
echo ""

total_tests=0
passed_tests=0
failed_tests=0

# Fonction pour exécuter un test
run_test() {
    local test_name="$1"
    local test_file="$2"
    local expected_pattern="$3"
    
    echo "─────────────────────────────────────────────────────────────"
    echo "Test: $test_name"
    echo "─────────────────────────────────────────────────────────────"
    
    if [ ! -f "$test_file" ]; then
        echo "✗ Fichier non trouvé: $test_file"
        ((failed_tests++))
        echo ""
        return 1
    fi
    
    output=$(clisp "$test_file" 2>&1)
    
    if echo "$output" | grep -q "$expected_pattern"; then
        echo "✓ SUCCÈS"
        ((passed_tests++))
    else
        echo "✗ ÉCHEC"
        echo "$output" | tail -20
        ((failed_tests++))
    fi
    echo ""
}

# Test 1: Tests VM
run_test "Tests VM" "run-vm-tests.lisp" "11.*11"

# Test 2: Tests Compiler
run_test "Tests Compiler" "run-compiler-tests.lisp" "32/32"

# Test 3: Tests Closures
output=$(clisp -x '(load "test-closures.lisp") (run-all-closure-tests)' 2>&1)
echo "─────────────────────────────────────────────────────────────"
echo "Test: Tests Closures"
echo "─────────────────────────────────────────────────────────────"
if echo "$output" | grep -q "10/10"; then
    echo "✓ SUCCÈS"
    ((passed_tests++))
else
    echo "✗ ÉCHEC"
    echo "$output" | tail -20
    ((failed_tests++))
fi
echo ""

# Test 4: Tests LABELS
run_test "Tests LABELS" "test-labels.lisp" "8 tests réussis"

# Calcul total
total_tests=$((passed_tests + failed_tests))

# Résumé
echo "═══════════════════════════════════════════════════════════════"
echo "RÉSUMÉ GLOBAL"
echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "Tests exécutés: $total_tests"
echo "Tests réussis:  $passed_tests"
echo "Tests échoués:  $failed_tests"
echo ""

if [ $failed_tests -eq 0 ]; then
    echo "✅ TOUS LES TESTS SONT PASSÉS!"
    echo ""
    echo "Détail des tests:"
    echo "  ✓ 11 tests VM"
    echo "  ✓ 32 tests Compiler"
    echo "  ✓ 10 tests Closures"
    echo "  ✓ 8 tests LABELS"
    echo ""
    echo "TOTAL: 61 tests unitaires (100%)"
    echo ""
    echo "Système complet avec 3 fichiers principaux:"
    echo "  • vm.lisp (Machine virtuelle)"
    echo "  • loader.lisp (Loader ASM)"
    echo "  • compiler.lisp (Compilateur LISP)"
    exit 0
else
    echo "❌ CERTAINS TESTS ONT ÉCHOUÉ"
    exit 1
fi

echo "═══════════════════════════════════════════════════════════════"
