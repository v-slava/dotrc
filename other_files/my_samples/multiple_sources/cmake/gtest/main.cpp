// see also:
// $DOTRC/other_files/build_or_install_scripts/not_used/install_gtest.sh

#include <gtest/gtest.h>

// TEST(customer_lib, DISABLED_success_on_some_request)
TEST(customer_lib, success_on_some_request)
{
    ASSERT_EQ(0, 4 - 4) << "something is wrong..." << 4 << ".";
    ASSERT_GT(4, 2) << "4 is not greater than 2. This assertion should pass.";
}

class FailurePrinter : public ::testing::EmptyTestEventListener
{
public:
    FailurePrinter(TestEventListener* listener) : EmptyTestEventListener()
    { _listener = listener; }

    void OnTestPartResult(const ::testing::TestPartResult& test_part_result)
    {
        if (test_part_result.failed())
        {
            std::cout << "[  FAILED  ] " << _test_case_name << "." << _name <<
            std::endl;
            _listener->OnTestPartResult(test_part_result);
        }
    }

    void OnTestStart(const ::testing::TestInfo& test_info)
    {
        _test_case_name = test_info.test_case_name();
        _name = test_info.name();
    }

    void OnTestIterationEnd(const ::testing::UnitTest& unit_test, int iteration)
    {
        _listener->OnTestIterationEnd(unit_test, iteration);
    }
    ~FailurePrinter() { delete _listener; }

protected:
    std::string _test_case_name;
    std::string _name;
    ::testing::TestEventListener* _listener;
};

namespace testing {
    namespace internal {
        extern bool g_help_flag;
    }
}

int main(int argc, char* argv[])
{
    // The following is a dirty hack to avoid printing help instead of executing
    // tests. From google test framework sources it seems that this variable
    // should be intialized as false. However in some cases according to gdb it
    // has value '246' here (no idea why). This excessive assignment solves the
    // problem.
    ::testing::internal::g_help_flag = false;

    ::testing::InitGoogleTest(&argc, argv);

    // Print failed tests only. Comment to use default printing.
    ::testing::TestEventListeners& listeners = testing::UnitTest::GetInstance()->listeners();
    ::testing::TestEventListener* listener = listeners.Release(listeners.default_result_printer());
    listeners.Append(new FailurePrinter(listener));

    return RUN_ALL_TESTS();
}
