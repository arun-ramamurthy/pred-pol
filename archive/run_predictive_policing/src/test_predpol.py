#!/usr/bin/env python
# -*- mode: python; fill-column: 79; comment-column: 50 -*-

# Unit Testing for Predpol
#
# Author(s):  PB
# Maintainer: PB, KL
# Created:    20161027
# License:    (c) HRDAG, GPL-v2 or greater
# ============================================
# Done:
#  -- conformity with PEP8: https://www.python.org/dev/peps/pep-0008/
#  -- TestCase for each approach
# todo:
# - add test for bins with 0 or 1 event
# - figure out why one test fails when we append 1 to pj

import unittest
import pandas as pd
import numpy as np
import predpol as pp


def run_EM(crime_data, start_date):
        ''' standard wrapper to get EM results '''
        last_crime_seen = max([max(crime_data[j])
                               for j in range(len(crime_data))])
        day_to_predict = last_crime_seen + pd.DateOffset(1)
        # training_period is `T` in the paper
        training_period = (last_crime_seen - start_date).days + 1
        rates, tops, omega, theta = pp.runEM(
            crime_data, training_period, day_to_predict, k=len(crime_data))
        return {'rates': rates, 'tops': tops, 'omega': omega, 'theta': theta}


class PredpolTestCase(unittest.TestCase):
    # crime_data is a dict of locations, ie., bins,
    # each of which has a vector of timestamps denoting crime occurrences.

    #t this code runs every time a test_ method is found
    def setUp(self):
        self.start_date = pd.datetime(2012, 1, 1)
        self.num_bins = 50
        self._gen_data()
        self.results = run_EM(self.crime_data, self.start_date)

    #t these two methods are here to indicate that subclasses need
    #t to define them.
    def _make_shifts(self):
        raise NotImplementedError

    def _gen_data(self):
        raise NotImplementedError

    def _test_theta(self):
        # note: all tests inherit this test unless overridden
        self.assertTrue(
            self.results['theta'] < 0.005,
            'theta should be negligible={}'.format(self.results['theta'])
        )


class PredpolTestEvenExact(PredpolTestCase):
    ''' expect precisely same predicted values in all bins'''

    #t this method gets called in PredpolTestCase.setUp. This class,
    #t PredpolTestEvenExact, is a subclass of PredpolTestCase, so it
    #t inherits setUp(). What I've done is change the behavior of the
    #t superclass's setUp.
    def _gen_data(self):
        self.crime_data = dict()
        self._make_shifts()
        for i in range(self.num_bins):
            self.crime_data[i] = [self.start_date + pd.DateOffset(shift) for
                                  shift in self.shifts]

    def _make_shifts(self):
        # made the time period longer to verify this gets closer to 1/3,
        # to avoid "edge effects"
        self.shifts = [j * 3 for j in range(100)]

    def test_rates(self):
        mean_rate = np.mean(self.results['rates'])
        # print(mean_rate)
        self.assertTrue(
            np.isclose(mean_rate, float(1/3.0), atol=0.005),
            'one crime every 3 days means mean rate == 0.33'.format(mean_rate)
        )
        min_rate = min(self.results['rates'])
        max_rate = max(self.results['rates'])
        self.assertTrue(
            np.isclose(min_rate, max_rate, atol=0.0001),
            'with same event pattern, rates should be equal'
        )
        #t _test_theta() is inherited from PredpolTestCase
        self._test_theta()


class PredpolTestEvenRough(PredpolTestEvenExact):
    ''' expect roughly same predicted values in all bins;
        same as EvenExact except different shifts.
    '''
    #t this class is a subclass of PredpolTestEvenExact, so it inherits
    #t _gen_data. That's good, because the only part that differs
    #t between PredpolTestEvenExact and PredpolTestEvenRough is the
    #t shifts. So here we redefine _make_shifts, which changes the
    #t behavior of PredpolTestEvenExact._gen_data(), and which in turn,
    #t changes the behavior of PredpolTestCase.doSetup
    def _make_shifts(self):
        self.shifts = sorted(np.random.randint(365, size=80))

    def test_rates(self):
        mean_rate = np.mean(self.results['rates'])
        self.assertTrue(
            np.isclose(mean_rate, float(80/365.0), atol=0.01),
            '80 events in 365 days should be close to mean'
        )
        max_rate = max(self.results['rates'])
        min_rate = min(self.results['rates'])
        max_diff_rate = max_rate - min_rate
        self.assertTrue(
            (max_diff_rate / max_rate) < 0.01,
            'abs rel diff of all min/max rates should be small'
        )
        self._test_theta()


class PredpolTestUneven(PredpolTestCase):
    ''' expect uneven rates and larger bin numbers to be ranked higher'''

    #t here we're redefining all of _gen_data. It still gets called in
    #t setUp because PredpolTestUneven is a subclass of PredpolTestCase.
    #t This is *not* a subclass of PredpolTestEvenExact, which is what the
    #t previous class was.
    def _gen_data(self):
        self.crime_data = dict()
        for i in range(self.num_bins):
            shifts = sorted(np.random.randint(365, size=i+2))
            self.crime_data[i] = [self.start_date + pd.DateOffset(shift)
                                  for shift in shifts]

    def test_rates(self):
        diffs = np.diff(self.results['rates'])
        self.assertTrue(
            all(r > 0 for r in diffs),
            'rates should increase monotonically: {}'.format(diffs)
        )
        expected_rates = [(i+2)/365.0 for i in range(self.num_bins)]
        self.assertTrue(
            np.allclose(self.results['rates'], expected_rates, atol=0.01),
            'rates should be proportional to bin sequence position'
        )
        self._test_theta()


class PredpolTestClustered(PredpolTestCase):
    ''' even w same baseline rate, more recent bins have higher rates
        each bin i has 10 events on days i, i+1, ..., i+9
    '''
    #t again we're inheriting from PredpolTestCase, so we need to define
    #t _gen_data.
    def _gen_data(self):
        self.crime_data = dict()
        for i in range(self.num_bins):
            bin_start = self.start_date + pd.DateOffset(i)
            self.crime_data[i] = [bin_start + pd.DateOffset(j)
                                  for j in range(10)]

    def test_rates(self):
        diffs = np.diff(self.results['rates'])
        self.assertTrue(
            all(r > 0 for r in diffs),
            'rates should increase monotonically'
        )
        self.assertTrue(
            self.results['theta'] > 0.1,
            'theta should be substantial={}'.format(self.results['theta'])
        )


class PredpolTestLambdafun(unittest.TestCase):
    def test_lambdafun(self):
        t = [pd.datetime(2012, 1, 1) + pd.DateOffset(i) for i in range(5)]
        self.assertEqual(1, pp.lambdafun(t, mu=1, theta=1, omega=0))

        lambdareturn = pp.lambdafun(t, mu=1, theta=1, omega=1)
        self.assertAlmostEqual(1.571, lambdareturn, places=2)

        lambdareturn = pp.lambdafun(t, mu=1, theta=0.5, omega=3)
        self.assertAlmostEqual(1.078, lambdareturn, places=2)


class PredpolTestCalctij(unittest.TestCase):
    #t as an exercise for the reader, we should create a superclass
    #t PredpolTestFrame, from which PredpolTestCalctij,
    #t PredpolTestCalcpij, PredpolTestEstep, and PredpolTestMstep, and
    #t prob PredpolTestLambdafun t would all inherit. This superclass
    #t would subclass unittest.TestCase, have a setUp() that defines
    #t start_date, t, and data.
    def test_calctij(self):
        ''' compare to hand-calced tij '''
        start_date = pd.datetime(2012, 1, 1)
        data = {0: [start_date + pd.DateOffset(i) for i in range(5)]}
        x = np.zeros((len(data[0])-1, len(data[0])-1))
        x[0, 0] = 1
        x[1, 0] = 2
        x[1, 1] = 1
        x[2, 0] = 3
        x[2, 1] = 2
        x[2, 2] = 1
        x[3, 0] = 4
        x[3, 1] = 3
        x[3, 2] = 2
        x[3, 3] = 1
        handtij = {0: x}
        tij = pp.calc_tij(data)
        self.assertTrue(np.array_equal(handtij[0], tij[0]))


class PredpolTestCalcpij(unittest.TestCase):
    ''' the pij values were hand-calcd in a spreadsheet '''
    def test_calcpij(self):
        #t note the repeated definition of start_date and data.
        start_date = pd.datetime(2012, 1, 1)
        data = {0: [start_date + pd.DateOffset(i) for i in range(5)]}
        x = np.zeros((len(data[0]) - 1, len(data[0]) - 1))
        x[0, 0] = 0.1839
        x[1, 0] = 0.0676
        x[1, 1] = 0.1839
        x[2, 0] = 0.0248
        x[2, 1] = 0.0676
        x[2, 2] = 0.1839
        x[3, 0] = 0.0091
        x[3, 1] = 0.0248
        x[3, 2] = 0.0676
        x[3, 3] = 0.1839
        handpij = {0: x}
        pij = pp.calc_pij(pp.calc_tij(data), theta=0.5, omega=1)
        self.assertTrue(
                np.allclose(handpij[0], pij[0], atol=0.001),
                'pij[0] is \n{}'.format(pij[0])
        )


class PredpolTestEstep(unittest.TestCase):
    def test_calc_estep(self):
        start_date = pd.datetime(2012, 1, 1)
        data = {0: [start_date + pd.DateOffset(i) for i in range(5)]}
        tij = pp.calc_tij(data)
        mu = {0: 1}
        pij, pj = pp.estep(data, mu=mu, theta=0.5, omega=1, tij=tij)
        # sum of each row in pij + pj (for each j) should == 1
        rowsums = np.sum(pij[0], axis=1)
        rowsmatch = [np.isclose(rowsums[j] + pj[0][j], 1, atol=0.005)
                     for j in range(4)]
        self.assertTrue(all(rowsmatch))

        handpij = np.zeros((len(data[0]) - 1, len(data[0]) - 1))
        handpij[0, 0] = 0.1554
        handpij[1, 0] = 0.0541
        handpij[1, 1] = 0.1469
        handpij[2, 0] = 0.0195
        handpij[2, 1] = 0.0530
        handpij[2, 2] = 0.1441
        handpij[3, 0] = 0.0071
        handpij[3, 1] = 0.0193
        handpij[3, 2] = 0.0526
        handpij[3, 3] = 0.1430
        self.assertTrue(np.allclose(handpij, pij[0], atol=0.005))

        handpj = [0.8446, 0.7990, 0.7833, 0.7778, 1.0]
        self.assertTrue(np.allclose(handpj, pj[0], atol=0.005))


class PredpolTestMstep(unittest.TestCase):
    def test_calc_mstep(self):
        start_date = pd.datetime(2012, 1, 1)
        data = {0: [start_date + pd.DateOffset(i) for i in range(5)]}
        last_crime_seen = max(data[0])
        training_period = (last_crime_seen - start_date).days + 1
        tij = pp.calc_tij(data)
        mu = {0: 1}
        pij, pj = pp.estep(data, mu=mu, theta=0.5, omega=1, tij=tij)
        omega, theta, mu = pp.mstep(pij, pj, tij, data, T=training_period)
        self.assertTrue(np.isclose(0.7544, omega, atol=0.01))
        self.assertTrue(np.isclose(0.1590, theta, atol=0.01))
        self.assertTrue(np.isclose(0.8410, mu[0], atol=0.01))


if __name__ == '__main__':
    unittest.main()
